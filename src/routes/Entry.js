import React, { PureComponent } from 'react'
import PropTypes from 'prop-types'
import { withRouter } from 'react-router-dom'
import { autobind } from 'core-decorators'
import urljoin from '../utils/url-join'

@withRouter
export default class extends PureComponent {

    static propTypes = {
        match: PropTypes.object.isRequired,
        location: PropTypes.object.isRequired,
        history: PropTypes.object.isRequired
    }

    state = {
        username: '',
        repo: '',
        apiKey: ''
    }

    @autobind
    go() {
        const { username, repo, apiKey } = this.state
        localStorage.setItem('apiKey', apiKey)
        this.props.history.push(
            urljoin(['/', username, repo])
        )
    }

    handleChange = key => evt => this.setState({
        [key]: evt.target.value
    })


    render() {
        const { username, repo, apiKey } = this.state

        return (
            <section>
                <input
                    value={username}
                    onChange={this.handleChange('username')} />
                <input
                    value={repo}
                    onChange={this.handleChange('repo')} />
                <textarea
                    value={apiKey}
                    onChange={this.handleChange('apiKey')} />
                <button
                    onClick={this.go}>
                    go
                </button>
            </section>
        )
    }
}