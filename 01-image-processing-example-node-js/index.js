require('dotenv').config()

const S3 = require('aws-sdk/clients/s3')
const sharp = require('sharp')

// Lambda function handler
const handler = (event, context, callback) => {
    const s3 = new S3({
        apiVersion: '2006-03-01', // latest
        accessKeyId: process.env.AWS_ACCESS_KEY_ID,
        secretAccessKey: process.env.AWS_SECRET_ACCESS_KEY
    })

    // For demonstration purposes we'll use a fixed object and bucket
    const demoImageName = 'test-image.jpg'
    const demoBucketName = 'serverless-haskell-test-bucket'

    s3.getObject({ Bucket: demoBucketName, Key: demoImageName }, (error, response) => {
        if (error) {
            // something bad happened
            callback(error, null)
            return
        }

        const imageData = response.Body

        // now, transform the image into a smaller size: 540px width
        sharp(imageData)
            .resize(540)
            .toBuffer()
            .then(data => {
                const uploadOptions = { Bucket: demoBucketName, Key: 'test-image-540.jpg', Body: data }
                s3.upload(uploadOptions, (error, data) => {
                    callback(null, true)
                })
            })
            .catch(error => callback(error, null))

        // then, transform the image into yet another smaller size

        // finally, upload the two new images
    })
}

handler({}, null, (error, result) => {
    console.log('Handler computation completed:')

    if (error) {
        console.error(error)
    }

    console.log(result)
})

exports.handler = handler
