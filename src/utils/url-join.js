import { compose, map, head, startsWith, replace } from 'ramda'

const urljoin = urls => {
    const absolute = compose(startsWith('/'), head)(urls)

    const normalized = map(compose(
        replace(/\/{2,}/g, ''),
        replace(/^(\/+)/, ''),
        replace(/(\/+)$/, ''),
    ), urls)

    const joined = normalized.filter(url => url !== '').join('/')

    return absolute ? `/${joined}` : joined
}

export default urljoin