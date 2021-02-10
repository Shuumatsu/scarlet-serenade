export const formatReadingTime = minutes => {
    let cups = Math.round(minutes / 5)
    let bowls = 0
    if (cups > 5) {
        return `${new Array(Math.round(cups / Math.E)).fill('🍱').join('')} ${minutes} min read`
    } else {
        return `${new Array(cups || 1).fill('☕️').join('')} ${minutes} min read`
    }
}

// `lang` is optional and will default to the current user agent locale
export const formatPostDate = (date, lang) => {
    if (typeof Date.prototype.toLocaleDateString !== 'function') {
        return date
    }

    date = new Date(date)
    const args = [lang, { day: 'numeric', month: 'long', year: 'numeric' }].filter(Boolean)
    return date.toLocaleDateString(...args)
}
