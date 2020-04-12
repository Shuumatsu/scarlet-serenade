import { Client } from 'fb-watchman'
import * as path from 'path'
import * as shell from 'shelljs'
import { existsSync } from 'fs'
import { promisify } from 'util'
import { docs_dir, pages_dir, md_css } from './paths'

const compile_doc = async (file: { name: string }) => {
    const absolute_filename = path.join(docs_dir, file.name)
    const dist_folder = path.join(pages_dir, path.dirname(file.name))
    if (!existsSync(dist_folder)) shell.mkdir('-p', dist_folder)
    const dist_filename = path.join(dist_folder, `${path.basename(file.name, '.md')}.html`)
    console.log(absolute_filename, dist_filename)

    if (shell.exec(`pandoc --css ${md_css} ${absolute_filename} -o ${dist_filename} -s --self-contained`).code != 0) {
        console.error(`pandoc failed to convert ${file.name} to html file.`)
    } else {
        console.error(`pandoc successfully converted ${file.name} to html file.`)
    }
}

compile_doc({ name: 'dead-lock.md' })

// const client = new Client()

// const command = promisify(client.command.bind(client))

// promisify(client.capabilityCheck.bind(client))({}).then(async () => {
//     const resp = await command(['watch-project', docs_dir])
//     if ('warning' in resp) {
//         console.log('warning: ', resp.warning)
//     }
//     console.log('watching ', resp.watch)

//     const docs_sub_name = 'docs_sub'
//     const docs_sub = {
//         // Match any `.js` file in the dir_of_interest
//         expression: ['allof', ['match', '*.md']],
//         // Which fields we're interested in
//         fields: ['name', 'size', 'mtime_ms', 'exists', 'type'],
//     }
//     await command(['subscribe', docs_dir, docs_sub_name, docs_sub])

//     client.on('subscription', resp => {
//         // console.log(resp)
//         if (resp.subscription !== docs_sub_name) return

//         for (const file of resp.files) {
//             // convert Int64 instance to javascript integer
//             const mtime_ms = +file.mtime_ms
//             console.log('file changed: ' + file.name, mtime_ms)

//             if (file.exists) compile_doc(file).catch(console.error)
//         }
//     })
// })
