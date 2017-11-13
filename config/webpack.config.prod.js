const CompressionPlugin = require('compression-webpack-plugin')
const MinifyPlugin = require('babel-minify-webpack-plugin')
const ExtractTextPlugin = require('extract-text-webpack-plugin')
const webpack = require('webpack')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const postcssConfig = require('./postcss.config.prod')
const paths = require('./paths')

module.exports = {
    entry: {
        ...paths.entries,
    },
    output: {
        path: paths.build,
        publicPath: paths.servedPath,
        pathinfo: true,
        filename: 'static/js/[name].[hash].js',
    },
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
            use: ExtractTextPlugin.extract({
                fallback: 'style-loader',
                use: [
                    { loader: 'css-loader' },
                    { loader: 'less-loader' }
                ]
            })
        }, {
            test: /\.module.css$/,
            include: paths.appSrc,
            use: ExtractTextPlugin.extract({
                fallback: 'style-loader',
                use: [
                    { loader: 'css-loader', options: { importLoaders: 1, module: true } },
                    { loader: 'postcss-loader', options: postcssConfig }
                ]
            })
        }, {
            test: /\.css$/,
            use: ExtractTextPlugin.extract({
                fallback: 'style-loader',
                use: [{ loader: 'css-loader' }]
            })
        }, {
            test: /\.(jpg|png|svg)$/,
            use: [{ loader: 'file-loader', options: { name: 'static/media/[name].[hash:8].[ext]' } }]
        }]
    },
    plugins: [
        new MinifyPlugin({}, { comments: false }),
        new webpack.optimize.CommonsChunkPlugin({
            children: true,
            async: true,
            minChunks: 3,
        }),
        new HtmlWebpackPlugin({
            inject: true,
            template: paths.html,
            minify: {
                removeComments: true,
                collapseWhitespace: true,
                removeRedundantAttributes: true,
                useShortDoctype: true,
                removeEmptyAttributes: true,
                removeStyleLinkTypeAttributes: true,
                keepClosingSlash: true,
                minifyJS: true,
                minifyCSS: true,
                minifyURLs: true
            },
            chunksSortMode: (chunk1, chunk2) => {
                const list = ['polyfills', 'vendor', 'app']
                var index1 = list.indexOf(chunk1.names[0])
                var index2 = list.indexOf(chunk2.names[0])
                if (index2 === -1 || index1 < index2) return -1
                if (index1 === -1 || index1 > index2) return 1
                return 0
            }
        }),
        new CompressionPlugin({
            asset: '[path].gz',
            algorithm: 'gzip',
            test: /\.js$|\.html|\.css$/,
            minRatio: 0.8
        })
    ]
}
