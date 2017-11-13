import React from 'react'
import 'normalize.css'
import { BrowserRouter, Route, Switch } from 'react-router-dom'
import { searchParamsToObject } from '../utils/searchParams'
import Entry from './Entry'
import Issues from './Issues'
import Issue from './Issue'
import Header from '../components/Header'

export default () => (
    <BrowserRouter>
        <Switch>
            <Route path='/:username/:repo/issues/:number' render={({ match, location }) => {
                const { username, repo, number } = match.params
                const parameters = searchParamsToObject(location.search)
                const props = { username, repo, number, parameters }

                return [
                    <Header base={false} key='header' />,
                    <Issue key='issue'  {...props} />
                ]
            }} />
            <Route path='/:username/:repo/issues' render={({ match, location }) => {
                const { username, repo } = match.params
                const parameters = searchParamsToObject(location.search)
                const props = { username, repo, parameters }

                return [
                    <Header base={true} key='header' />,
                    <Issues key='issues' {...props} />
                ]
            }} />
            <Route path='/:username/home' render={() => (
                <Header />
            )} />
            <Route path='/:username/:repo' render={() => (
                <Header />
            )} />
            <Route path='/' render={() => (
                <Entry />
            )} />
        </Switch>
    </BrowserRouter>
)
