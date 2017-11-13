import React, { PureComponent } from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { pickAll, compose, path, prop, map, equals } from 'ramda'
import { autobind } from 'core-decorators'
import InViewObserver from '../components/InViewObserver'
import { PENDING, SUCCESS, FAILURE } from '../constants'
import { tagName as Marked } from '../components/Marked'
import { issue, comments } from '../actions'

const issueArgumentsFromProps = pickAll(['username', 'repo', 'number'])
const getIssueTaskKey = compose(prop('taskKey'), issue, issueArgumentsFromProps)
const commentsArgumentsFromProps = pickAll(['username', 'repo', 'number', 'parameters'])
const getCommentsTaskKey = compose(prop('taskKey'), comments, commentsArgumentsFromProps)

const mapIssueState = (state, ownProps) => {
    const key = getIssueTaskKey(ownProps)
    const task = path(['tasks', key])(state) || {}

    if (task.status !== 'success')
        return { ...task }

    return {
        issue: path(['entities', 'issue', task.result])(state),
        issueError: task.error,
        issueStatus: task.status,
    }
}

const mapCommentsState = (state, ownProps) => {
    const key = getCommentsTaskKey(ownProps)
    const task = path(['tasks', key])(state) || {}

    if (task.status !== 'success')
        return { ...task }

    return {
        comments: map(commentsId =>
            path(['entities', 'comment', commentsId])(state)
            , task.result || []),
        commentsError: task.error,
        commentsStatus: task.status,
    }
}

const mapStateToProps = (state, ownProps) => ({
    ...mapCommentsState(state, ownProps),
    ...mapIssueState(state, ownProps)
})

const mapDispatchToProps = (dispatch, ownProps) => {
    return {
        fetchIssue: props => dispatch(issue(
            issueArgumentsFromProps(props)
        )),
        fetchComments: props => dispatch(comments(
            commentsArgumentsFromProps(props)
        )),
    }
}

@connect(mapStateToProps, mapDispatchToProps)
export default class extends PureComponent {

    state = {
        prepareComment: false
    }

    static propTypes = {
        fetchIssue: PropTypes.func.isRequired,
        fetchComments: PropTypes.func.isRequired,
        username: PropTypes.string.isRequired,
        repo: PropTypes.string.isRequired,
        number: PropTypes.string.isRequired,
        parameters: PropTypes.object,
        issue: PropTypes.object,
        commentsError: PropTypes.oneOf([PropTypes.object, PropTypes.string]),
        commentsStatus: PropTypes.oneOf([PENDING, SUCCESS, FAILURE]),
        issueError: PropTypes.oneOf([PropTypes.object, PropTypes.string]),
        issueStatus: PropTypes.oneOf([PENDING, SUCCESS, FAILURE])
    }

    @autobind
    updateIssue(props) {
        if (props.issueStatus !== SUCCESS
            && props.issueStatus !== PENDING)
            this.props.fetchIssue(props)
    }

    @autobind
    updateComments(props) {
        if (props.commentsStatus !== SUCCESS
            && props.commentsStatus !== PENDING)
            this.props.fetchComments(props)
    }

    @autobind
    commentsInViewHandler() {
        this.updateComments(this.props)
    }

    componentDidMount() {
        this.updateIssue(this.props)
    }

    componentWillReceiveProps(nextProps) {
        !equals(
            issueArgumentsFromProps(this.props),
            issueArgumentsFromProps(nextProps)
        ) && this.updateIssue(nextProps)

        !equals(
            commentsArgumentsFromProps(this.props),
            commentsArgumentsFromProps(nextProps)
        ) && this.updateComments(nextProps)
    }

    renderComments() {
        const { comments, commentsStatus, commentsError } = this.props
        if (commentsStatus === SUCCESS)
            return (
                <ol>
                    {map(comment => (
                        <li key={comment.id}>{comment.body}</li>
                    ), comments)}
                </ol>
            )

        if (commentsStatus === FAILURE)
            return <div>{commentsError}</div>

        return <div>Comments Loading...</div>
    }

    render() {
        const { issue, issueStatus, issueError } = this.props
        const { prepareComment } = this.state

        if (issueStatus === SUCCESS)
            return (
                [
                    <Marked key='Marked' raw={issue.body} />,
                    prepareComment && (
                        <InViewObserver key='comments' handler={this.commentsInViewHandler}>
                            {this.renderComments()}
                        </InViewObserver>
                    )
                ]
            )

        if (issueStatus === FAILURE)
            return <div>{issueError}</div>

        return <div>Loading...</div>
    }
}
