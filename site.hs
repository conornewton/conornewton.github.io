--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc
import           System.Environment
import           Control.Monad
import           System.FilePath
--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    let draftMode = length args == 2 && args !! 1 == "draft"
        postsPattern = if draftMode
                       then "posts/*.markdown" .||. "drafts/*.markdown"
                       else "posts/*.markdown"
        args' = take 1 args
    when draftMode $ putStrLn "Running in draft mode"
    withArgs args' $ hakyllWith (chooseConfig draftMode) $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler
        match postsPattern $ do
            route $ setExtension ".html" `composeRoutes` gsubRoute "drafts" (const "posts")
            compile $ compiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx >>= relativizeUrls
        match "programming.html" $ do
            route idRoute
            compile $ do
                let progCtx = constField "title" "Programming" `mappend`
                              defaultContext
                getResourceBody >>= applyAsTemplate progCtx
                                >>= loadAndApplyTemplate "templates/default.html" progCtx 
                                >>= relativizeUrls
        match "maths.html" $ do
            route idRoute
            compile $ do 
                getResourceBody >>= applyAsTemplate defaultContext
                                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                                >>= relativizeUrls
        create ["blog.html"] $ do
            route idRoute
            compile $ do 
                posts <- recentFirst =<< loadAll postsPattern
                let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                                 constField "title" "Archives"            `mappend`
                                 defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls
        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Home"                `mappend`
                        defaultContext
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls
        match "templates/*" $ compile templateBodyCompiler
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions {writerHTMLMathMethod = MathJax ""}

compiler :: Compiler (Item String)
compiler = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

draftConfig :: Configuration
draftConfig = defaultConfiguration {destinationDirectory = "build/preview", storeDirectory = "build/_cache/draft", tmpDirectory = "build/_cache/draft/tmp"}

finalConfig :: Configuration
finalConfig = defaultConfiguration {destinationDirectory = "build/final", storeDirectory = "build/_cache/final", tmpDirectory = "build/_cache/final/tmp"}

chooseConfig :: Bool -> Configuration
chooseConfig True  = draftConfig
chooseConfig False = finalConfig
