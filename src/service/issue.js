import { normalize, schema } from 'normalizr'
import urljoin from './utils/url-join'
import appendSearchParams from './utils/appendSearchParams'

const base = 'https://api.github.com/'

const issueSchema = new schema.Entity('issue')

// List issues for a repository
export const issues = async ({ owner, repo, parameters }) => {
    const url = appendSearchParams(
        urljoin([base, `/repos/${owner}/${repo}/issues`]),
        parameters
    )

    const init = new Headers({
        mode: 'cors',
        method: 'get'
    })

    const resp = await fetch(url, init)

    return normalize(await resp.json(), [issueSchema])
}

// List articles for a repository
export const articles = ({ owner, repo, parameters }) =>
    issues({ owner, repo, parameters: { ...parameters, labels: 'issue' } })

