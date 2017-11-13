export default string => {
    const template = document.createElement('template')
    template.innerHTML = string
    document.body.insertBefore(template, document.body.firstChild)

    return () => template.cloneNode(true)
}
