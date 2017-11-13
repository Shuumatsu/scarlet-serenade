const cssnext = require('postcss-cssnext')
const nano = require('cssnano')

module.exports = {
    plugins: () => [
        cssnext({
            browsers: [
                '>1%',
                'last 4 versions',
                'Firefox ESR',
                'not ie < 9', // React doesn't support IE8 anyway
            ],
            warnForDuplicates: false // cssnano uses autofixer too
        }),
        nano({
            browsers: [
                '>1%',
                'last 4 versions',
                'Firefox ESR',
                'not ie < 9', // React doesn't support IE8 anyway
            ]
        }),
    ]
}