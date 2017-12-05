-- Image Proccessing Example
-- A Use Case:
--     This example uses Haskell the AWS S3 api to take an image from an S3 bucket, transform it
--     into many new images of various sizes (thumbnail, mobile, --     tablet, etc.), gives the
--     images unique names, then upload the new images to the same S3 bucket.
--
-- How we'll do it:
--     With the name of the image, name of the S3 bucket, and the AWS api, we can retrieve the given image on S3.
--     Then, using image libraries, we can create new images of smaller sizes.
--     Finally, using the AWS api, we can upload the new images back to S3 in parallel
-- 
-- This example demonstrates:
--     - Interacting with a third-party service (the AWS api in this example)
--     - Image manipulation
--     - Monads


dummyImage = "test-image.jpg"
dummyBucket = "serverless-haskell-test-bucket"

main :: IO ()
main = do
    Prelude.putStrLn "hello world"
