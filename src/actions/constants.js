import { schema } from 'normalizr'

export const githubIdentifier = 'github-identifier'
export const githubEntityIdentifier = 'github-entity-identifier'
export const appIdentifier = 'app-identifier'
export const appEntityIdentifier = 'app-entity-identifier'
export const taskIdentifier = 'task-identifier'

// --- github

const userSchema = new schema.Entity('user')
const issueSchema = new schema.Entity('issue', {
    user: userSchema
})
const issuesSchema = [issueSchema]
const commentSchema = new schema.Entity('comment', {
    user: userSchema
})
const commentsSchema = [commentSchema]

export const githubSchemas = {
    userSchema,
    issueSchema,
    issuesSchema,
    commentSchema,
    commentsSchema
}

// ---