import uuidv1 from 'uuid/v1'
import { memoize, pickBy, contains, isNil } from 'ramda'
import taskTag from '../utils/taskTag'
import { githubIdentifier, githubSchemas } from './constants'

const {
    // userSchema,
    commentSchema,
    commentsSchema,
    issueSchema,
    issuesSchema
} = githubSchemas

const filterParameters = parametersFields => pickBy((key, value) =>
    contains(key, parametersFields) && !isNil(value)
)

export const issue = memoize(({ username, repo, number }) => {
    const taskKey = taskTag('issue', JSON.stringify)({
        username, repo, number
    }) || uuidv1()

    return {
        type: githubIdentifier,
        endpoint: `/repos/${username}/${repo}/issues/${number}`,
        method: 'get',
        schema: issueSchema,
        taskKey
    }
})

export const issues = memoize(({ username, repo, parameters }) => {
    const parametersFields = [
        'milestone', 'state', 'assignee',
        'creator', 'mentioned', 'labels',
        'sort', 'direction', 'since'
    ]
    const filterdParameters = filterParameters(parametersFields)(parameters)

    const taskKey = taskTag('issues', JSON.stringify)({
        username, repo, parameters: filterdParameters
    }) || uuidv1()

    return {
        type: githubIdentifier,
        endpoint: `/repos/${username}/${repo}/issues`,
        method: 'get',
        parameters: filterdParameters,
        schema: issuesSchema,
        taskKey
    }
})

export const comment = memoize(({ username, repo, number }) => {
    const taskKey = taskTag('comment', JSON.stringify)({
        username, repo, number
    }) || uuidv1()

    return {
        type: githubIdentifier,
        endpoint: `/repos/${username}/${repo}/issues/comments/${number}`,
        method: 'get',
        schema: commentSchema,
        taskKey
    }
})

export const comments = memoize(({ username, repo, number, parameters }) => {
    const parametersFields = ['since']
    const filterdParameters = filterParameters(parametersFields)(parameters)

    const taskKey = taskTag('comments', JSON.stringify)({
        username, repo, number, fparameters: filterdParameters
    }) || uuidv1()

    return {
        type: githubIdentifier,
        endpoint: `/repos/${username}/${repo}/issues/${number}/comments`,
        method: 'get',
        parameters: filterdParameters,
        schema: commentsSchema,
        taskKey
    }
})
