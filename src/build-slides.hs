import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory (getCurrentDirectory)
import qualified Data.List as L
import Control.Monad

baseTexInputs="/home/mattox/.fonts/:"
--webPath="/home/mattox/class/cs421-haskell/web/files"

nthFromEnd n xx = aux n (reverse xx)
  where aux 1 (x:xs) = x
        aux n (x:xs) | n > 1 = aux (n-1) xs

-- Add a latex directory to every dir in the cwd.
newTexInputs :: FilePath -> String
newTexInputs dir = L.intercalate ":" 
                 $ reverse  
                 $ map joinPath 
                 $ drop 1 
                 $ map (++ ["latex"]) 
                 $ L.inits 
                 $ splitDirectories dir

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    -- create a latex subdirectory off of every parent directory.
    slideFiles <- liftIO $ getDirectoryFilesIO "" ["//slides.tex"]
    want $ map (\ slides -> let slug = (takeFileName . dropTrailingPathSeparator . dropFileName) slides
                             in dropFileName slides ++ slug ++ ".pdf") slideFiles

--     phony "all" $ do
--         slideFiles <- getDirectoryFiles "" ["//slides.tex"]
--         putNormal $ show $ map 
   
--    let slug = head (reverse $ splitDirectories pwd)

--    want [slug ++ ".pdf"]

    phony "clean" $ do
        putNormal "Cleaning files...."
        removeFilesAfter "_build" ["//*"]
        cmd_ "rm -f slides.nav slides.snm *-slides.pdf"
        cmd_ "rm -rf _minted-slides"
        cmd "latexmk -C slides.tex"

--    phony "info" $ do
--        pwd <- liftIO $ getCurrentDirectory
--        putNormal $ "This is a slides directory named " ++ slug
--        putNormal $ "newTexInputs is " ++ newTexInputs

--    phony "stage" $ do
--        need ["slides.pdf", "slides-4up.pdf"]
--        pwd <- liftIO $ getCurrentDirectory
--        webPath <- filterM doesDirectoryExist
--                        ( reverse  
--                        $ map joinPath 
--                        $ drop 1 
--                        $ map (++ ["web","src","slides"]) 
--                        $ L.inits 
--                        $ splitDirectories pwd)
--        let wtarget = (head webPath)
--        let target = wtarget </> (slug ++ ".pdf")
--        let target4 = wtarget </> (slug ++ "-4up.pdf")
--        putNormal "Staging the slides!"
--        () <- cmd "cp slides.pdf " [target]
--        cmd "cp slides-4up.pdf " [target4]

    "//*.pdf" %> \out -> do
        pwd <- liftIO $ getCurrentDirectory
        let slug = dropExtension $ takeFileName out -<.> "pdf"
            cwd = (dropTrailingPathSeparator . dropFileName) out
            dirName = takeFileName cwd
            texInputs = newTexInputs (pwd </> cwd)
        if dirName /= slug
           then error $ "Directory name '" ++ dirName ++ "' mismatches file '" ++ slug
           else return ()
        need [dropFileName out ++ "slides.tex"]
        putNormal $ "Generating pdf for " ++ slug
        putNormal $ "in directory " ++ cwd
        putNormal $ "with texInputs " ++ texInputs
        cmd_ (Cwd cwd) (AddEnv "TEXINPUTS" $ texInputs ++ baseTexInputs) "latexmk -lualatex --shell-escape slides.tex"
        cmd (Cwd cwd) $ "mv slides.pdf " ++ slug ++ ".pdf"

--    (slug ++ ".pdf") %> \out -> do
--        need ["slides.tex"]
--        cmd_ (AddEnv "TEXINPUTS" (newTexInputs ++ ":" ++ texinputs)) "latexmk -lualatex --shell-escape slides.tex"
--        cmd $ "mv slides.pdf " ++ slug ++ ".pdf"
--
--
--    (slug ++ "-4up.pdf") %> \out -> do
--        need [slug ++ ".pdf"]
--        cmd $ "pdfnup --outfile " ++ slug ++ "-4up.pdf --nup 2x2 " ++ slug ++ ".pdf"
--
-- sh "TEXINPUTS=#{$texinputs} lualatex -shell-escape handout.tex"
