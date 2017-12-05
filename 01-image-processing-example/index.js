const { Readable } = require('stream')
const { spawn } = require('child_process')

// Lambda function handler
const handler = (event, context, callback) => {
    let result = ''
    const options = {
        input: JSON.stringify({
            imageName: 'test-image.jpg', bucketName: 'serverless-haskell-test-bucket', awsAccessId: '', awsSecretAccessKey: ''
        }),
        stdio: 'pipe'
    }
    const child = spawn('./Main', [], options)

    child.stdout.on('data', data => result += data)

    child.stderr.on('data', error => {
        console.error(`Child error:\n${error}`)
        callback(error, null)
        return
    })

    child.on('exit', code => {
        if (code === 0) {
            callback(null, result)
            return
        }

        callback("Error", null)
    })
}

handler({}, null, (error, result) => {
    console.log('Handler computation completed:')
    console.log(result)
})

exports.handler = handler
