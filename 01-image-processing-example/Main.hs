{-# LANGUAGE OverloadedStrings #-}
-- Image Proccessing Example
-- A Use Case:
--     This example uses Haskell the AWS S3 api to take an image from an S3 bucket, transform it
--     into a new images of some size (thumbnail, mobile, tablet, etc.), give the new
--     image unique name, then upload the new image to the same S3 bucket.
--
-- How we'll do it:
--     With the name of the image, name of the S3 bucket, and the AWS api, we can retrieve the given image on S3.
--     Then, using image libraries, we can create a new image of smaller size.
--     Finally, using the AWS api, we can upload the new image to S3
-- 
-- This example demonstrates:
--     - Interacting with a third-party service (the AWS api in this example)
--     - Image manipulation
--     - Monads

import Data.Text
import Data.ByteString.Char8
import Network.AWS -- amazonka
import Network.AWS.Data
import Network.AWS.S3
import Network.AWS.S3.Types
import qualified Graphics.Image as I
import Graphics.Image.Processing

-- TODO: update this example to to use a buffer (like the Node.js example), rather than read/write

main :: IO ()
main = do
    let dummyBucket = "serverless-haskell-test-bucket"
    let accessKey = ""
    let secretKey = ""
    getImage "test-image.jpg" dummyBucket accessKey secretKey
    -- scaleImage
    -- putImage "test-image-540.jpg" dummyBucket accessKey secretKey
    Prelude.putStrLn "Success!"

-- scaleImage resizes an image to 360px height and 540px width
scaleImage :: IO ()
scaleImage = do
    img <- I.readImageRGB I.VU "test-image.jpg"
    I.writeImage "test-image-540.jpg" $ Graphics.Image.Processing.resize Bilinear Edge (360, 540) img

-- getImage gets an image from S3
getImage :: String -> String -> String -> String -> IO ()
getImage imageName bucketName accessKey secretKey = do
    let packedAccessKey = Data.ByteString.Char8.pack accessKey
    let packedSecretKey = Data.ByteString.Char8.pack secretKey
    env <- newEnv $ FromKeys (AccessKey packedAccessKey) (SecretKey packedSecretKey)
    runResourceT $ runAWS env $ within Ohio $ do
        let s3BucketName = Network.AWS.S3.Types.BucketName (Data.Text.pack bucketName)
        let s3ImageName = Network.AWS.S3.Types.ObjectKey (Data.Text.pack imageName) -- (the filename to upload as)
        response <- send (Network.AWS.S3.getObject s3BucketName s3ImageName) -- get the file
        let destinationFilePath = "test-image.jpg"
        -- response `sinkBody` CB.sinkFile destinationFilePath -- save the file
        return ()
    return ()


-- putImage uploads and image to S3
putImage :: String -> String -> String -> String -> IO PutObjectResponse
putImage imageName bucketName accessKey secretKey = do
    let packedAccessKey = Data.ByteString.Char8.pack accessKey
    let packedSecretKey = Data.ByteString.Char8.pack secretKey
    env <- newEnv $ FromKeys (AccessKey packedAccessKey) (SecretKey packedSecretKey)
    requestBody <- chunkedFile defaultChunkSize imageName
    let s3BucketName = Network.AWS.S3.Types.BucketName (Data.Text.pack bucketName)
    let s3ImageName = Network.AWS.S3.Types.ObjectKey (Data.Text.pack imageName) -- (the filename to upload as)
    runResourceT $ runAWS env $ within Ohio $ send (Network.AWS.S3.putObject s3BucketName s3ImageName requestBody)


-- printBuckets :: IO ()
-- printBuckets = do
--     ex <- example
--     print $ [ x ^. bName | x <- ex ^. lbrsBuckets ]

-- example :: IO ListBucketsResponse
-- example = do
--     let accessKey = Data.ByteString.Char8.pack "key"
--     let secretKey = Data.ByteString.Char8.pack "key"
--     env <- newEnv $ FromKeys (AccessKey accessKey) (SecretKey secretKey)
--     Prelude.putStrLn "hello"
--     runResourceT . Network.AWS.runAWS env $ (Network.AWS.send Network.AWS.S3.listBuckets)


-- getFile :: Region -> BucketName -> ObjectKey -> IO ()
-- getFile region bucketName objectKey = do
--     -- lgr <- newLogger Debug stdout
--     let accessKey = Data.ByteString.Char8.pack "key"
--     let secretKey = Data.ByteString.Char8.pack "key"
--     env <- newEnv $ FromKeys (AccessKey accessKey) (SecretKey secretKey)
--     runResourceT . Network.AWS.runAWS env $ do
--         rs <- Network.AWS.send (getObject bucketName objectKey)
--         -- Prelude.putStrLn (show rs)
--         -- Prelude.putStrLn "Successfully download"
--         -- return(rs)
--         view gorsBody rs `sinkBody` CB.sinkFile f
--         say $ "Successfully Download: "
--             <> toText b <> " - " <> toText k <> " to " <> toText f
