module.exports = {
  presets: [
    [
      '@babel/preset-env',
      {
        targets: {
          node: 'current'
        }
      }
    ],
    ['@babel/preset-stage-0', { decoratorsLegacy: true }]
  ],
  plugins: [
    '@babel/plugin-proposal-class-properties',
    '@babel/plugin-proposal-pipeline-operator',
    '@babel/plugin-proposal-throw-expressions',
    '@babel/plugin-proposal-do-expressions'
  ]
}
