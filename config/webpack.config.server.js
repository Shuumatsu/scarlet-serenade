const paths = require('./paths')
const config = require('./webpack.config.dev')

module.exports = {
    headers: {
        'Access-Control-Allow-Origin': '*',
    },
    publicPath: config.output.publicPath,
    contentBase: paths.public,
    historyApiFallback: true,
    clientLogLevel: 'none',
    noInfo: true,
    quiet: true,
    stats: 'none',
    hot: true,
    open: true,
    watchOptions: {
        ignored: /node_modules/
    }
}
