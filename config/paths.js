const path = require('path')

const appDirectory = process.cwd()
const rootPathResolve = relativePath => path.resolve(appDirectory, relativePath)

const dotenv = rootPathResolve('.env')
require('dotenv').config({ path: dotenv })
const servedPath = process.env.PUBLIC_URL || '/'

const paths = {
    build: rootPathResolve('build'),
    public: rootPathResolve('public'),
    src: rootPathResolve('src'),
    entries: {
        polyfills: rootPathResolve('src/polyfills.js'),
        app: rootPathResolve('src/index.js')
    },
    html: rootPathResolve('public/index.html'),
    servedPath, dotenv
}

module.exports = paths