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

-- import Data.ByteString.Internal.ByteString
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
-- import           Control.Monad.Trans.AWS
-- import Network.AWS -- amazonka
import Data.ByteString.Char8
-- import Network.AWS.Data
-- import           Control.Monad.Trans.AWS
-- import Network.AWS.S3 -- amazonka-s3
-- import Network.AWS.S3.GetObject as S3GetObject
-- import Network.AWS.S3.PutObject as S3PutObject
-- import Network.AWS.S3.CreateMultipartUpload
import qualified Graphics.Image as I
import Graphics.Image.Processing

-- bufferToImageRepresentation gets the (ith, jth) pixel of an image in a buffer
-- bufferToImageRepresentation :: IO ()
-- bufferToImageRepresentation i j = putStrLn "hello world"

createImage :: IO ()
createImage = putStrLn "TODO"

-- scaleImage takes an image from disk, transforms, and saves the image back to disk
-- TODO: design this to take a buffer
scaleImage :: IO ()
scaleImage = do
    img <- I.readImageRGB I.VU "test-image.jpg"
    I.writeImage "test-image-540.jpg" $ Graphics.Image.Processing.resize Bilinear Edge (360, 540) img

main :: IO ()
main = do
    let dummyImage = "test-image.jpg"
    let dummyBucket = "serverless-haskell-test-bucket"
    Prelude.putStrLn "Connecting to the aws api"


-- resize transforms an image to a new size given the image buffer and the new size for the image
-- resize :: Image -> Int -> String
-- resize imageBuffer newSize = "foobar"

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

-- retrieveObject :: Region -> BucketName -> String
-- retrieveObject r b = "Foobar"


-- say :: MonadIO m => Text -> m ()
-- say = liftIO . Text.putStrLn
