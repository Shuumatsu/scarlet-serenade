import { normalize, schema } from 'normalizr'
import urljoin from './utils/url-join'
import appendSearchParams from './utils/appendSearchParams'

const base = 'https://api.github.com/'

const userSchema = new schema.Entity('user')
const commitSchema = new schema.Entity('commit', {
    user: userSchema,
}, { idAttribute: 'version' })
const forkSchema = new schema.Entity('fork', {
    user: userSchema
})
const gistSchema = new schema.Entity('gist', {
    owner: userSchema,
    history: [commitSchema],
    forks: [forkSchema]
})

// List public gists for the specified user
export const gists = async ({ username, parameters }) => {
    const url = appendSearchParams(
        urljoin([base, `/users/${username}/gists`]),
        parameters
    )

    const init = new Headers({
        mode: 'cors',
        method: 'get'
    })

    const resp = await fetch(url, init)

    return normalize(await resp.json(), [gistSchema])
}

// gists({ username: 'vanishingdante' }).then(console.log)

// Get a single gist
// sha for a specific revision 
export const gist = async ({ id, sha }) => {
    const url = urljoin([base, sha === undefined ? `/gists/${id}` : `/gists/${id}/${sha}`])

    const init = new Headers({
        mode: 'cors',
        method: 'get'
    })

    const resp = await fetch(url, init)

    return normalize(await resp.json(), gistSchema)
}

// gist({ id: '2c95ab6d87d2fd9bd2a411a4526af2e3' }).then(console.log)


// Create a gist
// Note: Don't name your files"gistfile" with a numerical suffix. This is the format of the automatic naming scheme that Gist uses internally.
export const create = async ({ files, description, _public }) => {
    const url = urljoin([base, 'gists'])

    const init = new Headers({
        mode: 'cors',
        method: 'post',
        // public is a reserved word
        body: { files, description, 'public': _public }
    })

    const resp = await fetch(url, init)

    return normalize(await resp.json(), gistSchema)
}

export const edit = async ({ id, files, description, _public }) => {
    const url = urljoin([base, `gists/${id}`])

    const init = new Headers({
        mode: 'cors',
        method: 'patch',
        // public is a reserved word
        body: { files, description, 'public': _public }
    })

    const resp = await fetch(url, init)

    return normalize(await resp.json(), gistSchema)
}

// Delete a gist
// delete is a reserved word
export const _delete = async ({ id }) => {
    const url = urljoin([base, `gists/${id}`])

    const init = new Headers({
        mode: 'cors',
        method: 'delete',
    })

    const resp = await fetch(url, init)

    return resp.status
}
