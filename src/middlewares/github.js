import { normalize } from 'normalizr'
import { camelizeKeys } from 'humps'
import { prop, flatten, contains } from 'ramda'
import { PENDING, SUCCESS, FAILURE } from '../constants'
import {
    githubIdentifier,
    githubEntityIdentifier,
    taskIdentifier
} from '../actions/constants'
import urljoin from '../utils/url-join'
import appendSearchParams from '../utils/appendSearchParams'

const API_ROOT = 'https://api.github.com/'

export default store => next => async action => {
    const { type } = action
    if (!contains(githubIdentifier, flatten([type])))
        return next(action)

    const { endpoint, schema, method, parameters, body } = action
    const { taskKey } = action

    const url = appendSearchParams(
        urljoin([API_ROOT,
            typeof endpoint === 'function' ?
                endpoint(store.getState()) :
                endpoint
        ]),
        typeof parameters === 'function' ?
            parameters(store.getState()) :
            parameters
    )

    next({ taskKey, type: [taskIdentifier], status: PENDING })

    const init = {
        mode: 'cors',
        headers: {
            Accept: 'application/vnd.github.v3+json',
            Authorization: 'token 682b782b3dee62da3a2b304405060319b82051d3'
        },
        method,
        body
    }

    try {
        const response = await fetch(url, init)
        const json = await response.json()
        if (!response.ok)
            throw json

        const camelizedJson = camelizeKeys(json)
        const normalized = normalize(camelizedJson, schema)

        next({
            type: [taskIdentifier, githubEntityIdentifier],
            taskKey,
            status: SUCCESS,
            entities: prop('entities', normalized),
            result: normalized.result
        })
    } catch (err) {
        next({
            type: [taskIdentifier],
            taskKey,
            status: FAILURE,
            error: err.message || `Something bad happened when processing action: ${JSON.stringify(action)}`
        })
    }
}
