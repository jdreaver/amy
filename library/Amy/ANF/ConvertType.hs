module Amy.ANF.ConvertType
  ( convertANFType
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Amy.ANF.AST as ANF
import Amy.Syntax.AST as S

-- | Converts a 'S.Type' into an ANF 'ANF.Type'
--
-- The only trickiness here is we have to know how type constructors are
-- represented in order to properly convert a type.
--
convertANFType :: Map TyConName ANF.Type -> S.Type -> ANF.Type
convertANFType tyConReps ty = go (unfoldTyFun ty)
 where
  lookupTyConRep :: TyConName -> ANF.Type
  lookupTyConRep tyCon =
    fromMaybe (error $ "Couldn't find ANF Type for unknown TyCon " ++ show tyCon)
    $ Map.lookup tyCon tyConReps
  go :: NonEmpty S.Type -> ANF.Type
  go (ty' :| []) =
    case ty' of
      S.TyUnknown -> error "Encountered TyUnknown in convertType"
      S.TyCon (MaybeLocated _ con) -> lookupTyConRep con
      S.TyVar _ -> OpaquePointerType
      S.TyExistVar _ -> error "Found TyExistVar in Core"
      app@S.TyApp{} ->
        case unfoldTyApp app of
          TyCon (MaybeLocated _ con) :| _ -> lookupTyConRep con
          _ -> error $ "Can't convert non-TyCon TyApp yet " ++ show ty'
      -- N.B. ANF/LLVM doesn't care about polymorphic records
      S.TyRecord rows _ -> mkRecordType tyConReps rows
      S.TyFun{} -> ClosureType
      S.TyForall _ ty'' -> convertANFType tyConReps ty''
  go _ = ClosureType

mkRecordType :: Map TyConName ANF.Type -> Map (MaybeLocated RowLabel) S.Type -> ANF.Type
mkRecordType tyConReps rows =
  RecordType $ flip fmap (Map.toAscList rows) $ \(MaybeLocated _ label, ty) -> (label, convertANFType tyConReps ty)
