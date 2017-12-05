# 01-image-processing-example

## Use Case

This example uses Haskell the AWS S3 api to take an image from an S3 bucket, transform it into many new images of various sizes (thumbnail, mobile, tablet, etc.), gives the images unique names, then upload the new images to the same S3 bucket.

This function could be triggered whenever a new object is uploaded to a specific S3 bucket.

## Getting started with this example

For this example, we use an image named `test-image.jpg` on a S3 bucket named `serverless-haskell-test-bucket`. Be sure the AWS credentials you're using have permissions to read and write objects to your bucket.
