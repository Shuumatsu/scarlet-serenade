import React, { PureComponent } from 'react'
import PropTypes from 'prop-types'
import { Link } from 'react-router-dom'
import { connect } from 'react-redux'
import { autobind } from 'core-decorators'
import { pickAll, path, compose, prop, map, equals } from 'ramda'
import { PENDING, SUCCESS, FAILURE } from '../constants'
import { issues } from '../actions'

const issuesArgumentsFromProps = pickAll(['username', 'repo', ' parameters'])
const getTaskKey = compose(prop('taskKey'), issues, issuesArgumentsFromProps)

const mapStateToProps = (state, ownProps) => {
    const key = getTaskKey(ownProps)
    const task = path(['tasks', key])(state) || {}

    return {
        error: task.error,
        status: task.status,
        issues: map(issueId =>
            path(['entities', 'issue', issueId])(state)
            , task.result || [])
    }
}

const mapDispatchToProps = (dispatch, ownProps) => {
    return {
        fetchIssues: props => dispatch(issues(
            issuesArgumentsFromProps(props)
        ))
    }
}

@connect(mapStateToProps, mapDispatchToProps)
export default class extends PureComponent {

    static propTypes = {
        username: PropTypes.string.isRequired,
        repo: PropTypes.string.isRequired,
        fetchIssues: PropTypes.func.isRequired,
        issues: PropTypes.arrayOf(PropTypes.object),
        error: PropTypes.oneOf([PropTypes.object, PropTypes.string]),
        status: PropTypes.oneOf([PENDING, SUCCESS, FAILURE])
    }

    @autobind
    update(props) {
        if (props.status !== SUCCESS
            && props.state !== PENDING)
            this.props.fetchIssues(props)
    }

    componentDidMount() {
        this.update(this.props)
    }

    componentWillReceiveProps(nextProps) {
        if (!equals(
            issuesArgumentsFromProps(this.props),
            issuesArgumentsFromProps(nextProps)
        )) this.update(nextProps)
    }

    render() {
        const { issues, error, status } = this.props
        if (status === SUCCESS)
            return (
                <section>
                    <ul>
                        {issues.map(issue => (
                            <li key={issue.id}>
                                <p>{issue.title}</p>
                                <Link to={`${issue.number}`}>issue</Link>
                            </li>
                        ))}
                    </ul>
                </section >
            )

        if (status === FAILURE)
            return <div>{error}</div>

        return <div>Loading...</div>
    }
}