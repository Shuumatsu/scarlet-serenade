import Typography from 'typography'

const defaultFonts =
    'system-ui, -apple-system, BlinkMacSystemFont, segoe ui, Roboto, Helvetica,Arial, sans-serif,apple color emoji, segoe ui emoji, segoe ui symbol'

const config = {
    baseFontSize: '16px',
    baseLineHeight: 1.5,
    scaleRatio: 2.5,
    headerFontFamily: ['Google Sans', 'Noto Sans SC', defaultFonts],
    bodyFontFamily: ['Google Sans', 'Noto Sans SC', defaultFonts],
    includeNormalize: false,
    overrideThemeStyles: ({ rhythm }, options, styles) => ({
        'wired-card': {
            // keep it the same with figure
            marginBottom: '1.5rem',
        },
        ol: {
            marginLeft: '2rem',
        },
    }),
}

const typography = new Typography(config)

export default typography
