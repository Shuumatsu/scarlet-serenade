const path = require('path')
const fsExtra = require('fs-extra')
const webpack = require('webpack')
const chalk = require('chalk')
const gzipSize = require('gzip-size')
const { filter, endsWith } = require('ramda')
const paths = require('../config/paths')
const config = require('../config/webpack.config.prod')

require('dotenv').config({ path: paths.dotenv })
process.env.NODE_ENV = process.env.NODE_ENV || 'production'

const processEnvForDefinePlugin = {}
for (let key in process.env) processEnvForDefinePlugin[key] = JSON.stringify(process.env[key])
config.plugins.unshift(new webpack.DefinePlugin({
    'process.env': processEnvForDefinePlugin
}))


fsExtra.removeSync(paths.build)
fsExtra.copySync(paths.public, paths.build, {
    dereference: true,
    filter: file => file !== paths.html
})

const printAssets = assets => {
    const notGzip = filter(asset => {
        const name = asset.name
        if (endsWith('.gz', name))
            return false
        return true
    }, assets)

    console.log()
    for (const asset of notGzip) {
        const filePath = path.resolve(paths.build, asset.name)
        const content = fsExtra.readFileSync(filePath)
        const gzippedSize = gzipSize.sync(content)

        let dirname = path.dirname(asset.name)
        dirname = dirname === '.' ? '' : `${dirname}/`
        const basename = path.basename(asset.name)

        console.log(dirname + chalk.cyan(basename))
        console.log(` - gzipped size: ${gzippedSize}`)
        console.log(` - size on disk: ${asset.size}`)
    }
    console.log()
}

const handleCompile = (err, stats) => {
    if (err) {
        console.error(err.stack || err)
        err.details && console.error(err.details)
        return
    }

    console.log(new Date())

    const info = stats.toJson()

    if (stats.hasErrors()) {
        console.log(chalk.red('Failed to compile.\n'))
        info.errors.forEach(message => console.log(message))
        return
    }

    console.log(chalk.green('Compiled successfully!'))

    printAssets(info.assets)

    if (stats.hasWarnings()) {
        console.log(chalk.yellow('Compiled with warnings.\n'))
        info.warnings.forEach(message => console.log(message))
    }
}

const compiler = webpack(config)

compiler.run(handleCompile)