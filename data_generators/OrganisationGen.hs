{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

-- | Generate data for the "organisation" schema used in Cheney et al.'s 2013 SIGMOD paper 
-- on query shredding.
module Main where

import           Text.Printf
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy  as B
import qualified Data.Csv              as C
import qualified Data.Foldable         as F
import           Data.Vector           ((!))
import qualified Data.Vector           as V
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.Random.MWC
import qualified Data.Text as T

newtype Client = Client Bool

instance C.ToField Client where
    toField (Client True)  = "true"
    toField (Client False) = "false"

instance Variate Client where
    uniform g = Client <$> uniform g
    uniformR (Client b1, Client b2) g = Client <$> uniformR (b1, b2) g

--------------------------------------------------------------------------------

data Options = Options
    { o_dpts      :: !Int         -- ^ Number of departments
    , o_gen       :: GenIO
    }

mkDefaultOptions :: IO Options
mkDefaultOptions = do
    gen    <- withSystemRandom $ asGenIO return

    return $ Options { o_dpts       = 100
                     , o_gen        = gen
                     }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['d'] ["departments"]
             (ReqArg (\ds opts -> return $ opts { o_dpts = read ds } ) "DEPARTMENTS")
             "number of departments"
    ]

parseOptions :: [String] -> IO Options
parseOptions argv =
    case getOpt Permute options argv of
        (o, [], [])  -> mkDefaultOptions >>= \defOpt -> F.foldlM (flip id) defOpt o
        (_, _, errs) -> ioError (userError $ concat errs ++ usageInfo usage options)
  where
    usage = "Usage: dptgen OPTIONS"

--------------------------------------------------------------------------------

type DptGen a = ReaderT Options IO a

type Key = Int
type Salary = Int

-- | Make a text value unique by appending a unique integer
textKey :: Int -> T.Text -> T.Text
textKey k t = T.append t (T.pack $ show k)

vectorIndex :: V.Vector a -> DptGen Int
vectorIndex v = asks o_gen >>= uniformR (0, V.length v - 1)

randomName :: DptGen T.Text
randomName = do
    let names = [ "alan", "bert", "charlie", "david", "edward"
                , "alice", "betty", "clara", "dora", "eve" ]
    (names !) <$> vectorIndex names

writeDepts :: DptGen (V.Vector T.Text)
writeDepts = do
    dpts <- mkDepts
    f <- printf "departments%d.csv" <$> asks o_dpts
    lift $ withFile f WriteMode $ \h -> B.hPut h (C.encode $ V.toList dpts)
    return $ fmap snd dpts

mkDepts :: DptGen (V.Vector (Key, T.Text))
mkDepts = asks o_dpts >>= \n -> V.generateM n mkDept

mkDept :: Key -> DptGen (Key, T.Text)
mkDept k = do
    let rawDptNames = ["Sales", "Research", "Quality", "Product"]
    d <- (rawDptNames !) <$> vectorIndex rawDptNames
    return $ (k, textKey k d)

--------------------------------------------------------------------------------

-- | Create n * 100 employees and assign them to random departments
mkEmps :: V.Vector T.Text -> DptGen (V.Vector (Key, T.Text, T.Text, Salary))
mkEmps dpts = asks o_dpts >>= \n -> V.generateM (n * 100) (mkEmp dpts)

mkEmp :: V.Vector T.Text -> Key -> DptGen (Key, T.Text, T.Text, Salary)
mkEmp dpts k = do
    let salaries = [700,900,20000,50000,60000,100000,2000000]
    d <- (dpts !) <$> vectorIndex dpts
    n <- randomName
    s <- (salaries !) <$> vectorIndex salaries
    return (k, d, textKey k n, s)

writeEmps :: V.Vector T.Text -> DptGen (V.Vector T.Text)
writeEmps dpts = do
    emps <- mkEmps dpts
    f <- printf "employees%d.csv" <$> asks o_dpts
    lift $ withFile f WriteMode $ \h -> B.hPut h (C.encode $ V.toList emps)
    return $ fmap (\(_, _, n, _) -> n) emps

writeTasks :: V.Vector T.Text -> DptGen ()
writeTasks emps = do
    tasks <- mapM mkTasks $ V.toList emps
    keyedTasks <-sequence $ zipWith ($) (concat tasks) [1..]
    f <- printf "tasks%d.csv" <$> asks o_dpts
    lift $ withFile f WriteMode $ \h -> B.hPut h (C.encode keyedTasks)

-- | Generate between one to four tasks for one employee
mkTasks :: T.Text -> DptGen [Key -> DptGen (Key, T.Text, T.Text)]
mkTasks emp = do
    nrTasks <- asks o_gen >>= uniformR (1::Int,4)
    return $ map (\_ -> mkTask emp) [1..nrTasks]

mkTask :: T.Text -> Key -> DptGen (Key, T.Text, T.Text)
mkTask emp k = do
    let tasks = ["abstract", "buy", "call", "dissemble", "enthuse"]
    t <- (tasks !) <$> vectorIndex tasks
    return (k, emp, t)

-- | Create n * 100 * 10 contacts (in chunks of 1000) and assign them to random
-- departments
writeContacts :: V.Vector T.Text -> DptGen ()
writeContacts dpts = do
    n <- asks o_dpts
    let f = printf "contacts%d.csv" n
    h <- lift $ openFile f WriteMode


    forM_ ([1..n] :: [Int]) $ \i -> mkContacts (i * 1000) dpts h
    lift $ hClose h

mkContacts :: Int -> V.Vector T.Text -> Handle -> DptGen ()
mkContacts k dpts h = do
    cs <- forM [(k::Int)+1..k+1000] (mkContact dpts)
    lift $ B.hPut h (C.encode cs)

mkContact :: V.Vector T.Text -> Key -> DptGen (Key, T.Text, T.Text, Client)
mkContact dpts k = do
    c <- asks o_gen >>= uniform
    d <- (dpts !) <$> vectorIndex dpts
    n <- randomName
    return (k, n, d, c)

genOrganisation :: DptGen ()
genOrganisation = do
    dpts <- writeDepts
    writeContacts dpts
    emps <- writeEmps dpts
    writeTasks emps

--------------------------------------------------------------------------------

main :: IO ()
main = do
    argv <- getArgs
    opts <- parseOptions argv
    runReaderT genOrganisation opts
