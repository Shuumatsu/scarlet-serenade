const path = require('path')

const pathPrefix = '/scarlet-serenade'

const siteMetadata = {
    siteUrl: 'https://shuumatsu.github.io/scarlet-serenade',
    title: 'Scarlet Serenade',
    description: 'implementing...',
}

const plugins = [
    'gatsby-transformer-unimd',
    {
        resolve: 'gatsby-plugin-typography',
        options: {
            pathToConfigModule: `src/styles/typography`,
            omitGoogleFont: true,
        },
    },
    'gatsby-plugin-offline',
    {
        resolve: 'gatsby-plugin-manifest',
        options: {
            icon: 'src/images/icon.png',
        },
    },
    'gatsby-plugin-sitemap',
    'gatsby-plugin-sass',
    'gatsby-plugin-react-helmet',
    'gatsby-transformer-sharp',
    {
        resolve: 'gatsby-source-filesystem',
        options: {
            name: 'images',
            path: './src/images/',
        },
        __key: 'images',
    },
    {
        resolve: 'gatsby-source-filesystem',
        options: {
            name: 'pages',
            path: './src/pages/',
        },
        __key: 'pages',
    },
    {
        resolve: 'gatsby-source-filesystem',
        options: {
            name: 'posts',
            path: './src/posts/',
        },
        __key: 'posts',
    },
    {
        resolve: 'gatsby-plugin-page-creator',
        options: {
            path: './src/posts/',
        },
    },
]

module.exports = { pathPrefix, siteMetadata, plugins }
