{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Molecule
  ( getMoleculeR
  , postMoleculeR
  ) where

import Import
import Data.Conduit(($$))
import Data.Conduit.List(consume)
import Data.Text.Encoding(decodeUtf8)
import Ouch.Input.Smiles(readSmi)
import Ouch.Output.Smiles(writeSmiles)
import Ouch.Property.Composition(atomCount, molecularFormula, molecularWeight)
import Ouch.Structure.Molecule(Molecule)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Ouch.Property.Builder as OPB
import Text.Printf(printf)

getMoleculeR :: String -> Handler RepHtml
getMoleculeR cmd = do
    let moleculesAndFilename =
          case cmd of
            "sample" -> Just (sampleMolecules, "")
            "upload" -> Nothing :: Maybe ([Molecule], Text)
            _        -> Nothing :: Maybe ([Molecule], Text)
    (formWidget, formEnctype) <- generateFormPost importForm
    defaultLayout $ do
        $(widgetFile "molecule")

postMoleculeR :: String -> Handler RepHtml
postMoleculeR cmd = do
    ((result, formWidget), formEnctype) <- runFormPost importForm
    moleculesAndFilename <- processFile result
    defaultLayout $ do
        $(widgetFile "molecule")

importForm :: Form FileInfo
importForm = renderDivs $ fileAFormReq "Choose a SMILES file:"

processFile :: FormResult FileInfo -> Handler (Maybe ([Molecule], Text))
processFile formRes =
    case formRes of
      FormSuccess fi -> do
        molecules <- lift $ toMolecules <$> (fileSource fi $$ consume)
        let fName = fileName fi
        return . Just $ (molecules, fName)
      _ -> return Nothing
  where
    toMolecules :: [B.ByteString] -> [Molecule]
    toMolecules = map (readSmi . T.unpack) . smiles
      where
        smiles :: [B.ByteString] -> [Text]
        smiles = T.lines . decodeUtf8 . B.concat

showMolForm :: Molecule -> String
showMolForm = showProperty molecularFormula

showAtomCount :: Molecule -> String
showAtomCount = showProperty atomCount

showMolWeight :: Molecule -> String
showMolWeight mol = let (OPB.DoubleValue d) = getPropertyValue molecularWeight mol
                     in printf "%.2f\n" d

getPropertyValue :: OPB.Property -> Molecule -> OPB.Value
getPropertyValue prop m = case OPB.value prop of
                            Left  v -> v
                            Right f -> f m

showProperty :: OPB.Property -> Molecule -> String
showProperty p m = show $ getPropertyValue p m

-- Sample set of compounds
sampleMolecules :: [Molecule]
sampleMolecules = map readSmi
    [ "[H]C([H])([H])[H]"
    , "[H]N([H])[H]"
    , "[H]O[H]"
    , "[H][N+]([H])([H])[H]"
    , "F[H]"
    , "C=C"
    , "CC"
    , "OO"
    , "[OH-].[Na+]"
    , "O=C=O"
    ]

