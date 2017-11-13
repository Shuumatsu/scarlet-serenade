const webpack = require('webpack')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const paths = require('./paths')
const postcssConfig = require('./postcss.config.dev')

module.exports = {
    entry: {
        ...paths.entries,
        app: [
            'webpack-hot-middleware/client',
            paths.entries.app
        ]
    },
    output: {
        path: paths.build,
        publicPath: paths.servedPath,
        pathinfo: true,
        filename: 'static/js/[name].js',
    },
    stats: 'none',
    resolve: {
        extensions: ['.js', '.jsx', '.json'],
    },
    module: {
        rules: [{
            test: /\.(js|jsx)$/,
            use: [{ loader: 'eslint-loader' }],
            include: paths.appSrc,
            enforce: 'pre'
        }, {
            test: /\.json$/,
            use: [{ loader: 'json-loader' }]
        }, {
            test: /-worker\.js$/,
            include: paths.appSrc,
            use: [{ loader: 'babel-loader' }, { loader: 'worker-loader' }],
        }, {
            test: /\.(js|jsx)$/,
            exclude: /-worker\.js$/,
            include: paths.appSrc,
            use: [{ loader: 'babel-loader', options: { cacheDirectory: true } }]
        }, {
            test: /\.less$/,
            use: [
                { loader: 'style-loader' },
                { loader: 'css-loader' },
                { loader: 'less-loader' }
            ]
        }, {
            test: /\.module.css$/,
            include: paths.appSrc,
            use: [
                { loader: 'style-loader' },
                { loader: 'css-loader', options: { importLoaders: 1, module: true } },
                { loader: 'postcss-loader', options: postcssConfig }
            ]
        }, {
            test: /\.css$/,
            use: [
                { loader: 'style-loader' },
                { loader: 'css-loader' }
            ]
        }, {
            test: /\.(jpg|png|svg)$/,
            use: [{ loader: 'file-loader', options: { name: 'static/media/[name].[hash:8].[ext]' } }]
        }]
    },
    plugins: [
        new webpack.HotModuleReplacementPlugin(),
        // prints more readable module names in the browser console on HMR updates
        new webpack.NamedModulesPlugin(),
        new HtmlWebpackPlugin({
            inject: true,
            template: paths.html,
            chunksSortMode: (chunk1, chunk2) => {
                const list = ['polyfills', 'vendor', 'app']
                var index1 = list.indexOf(chunk1.names[0])
                var index2 = list.indexOf(chunk2.names[0])
                if (index2 === -1 || index1 < index2) return -1
                if (index1 === -1 || index1 > index2) return 1
                return 0
            }
        })
    ],
    devtool: 'cheap-module-source-map'
}
