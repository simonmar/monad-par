{-# LANGUAGE DeriveDataTypeable,CPP,FlexibleInstances,UndecidableInstances,ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | This module provides the 'Serializable' type class and functions
-- to convert to and from 'Payload's. It's implemented in terms of the
-- @cereal@ package's "Data.Serialize". 
module RPC.Encoding (
          Serializable,
          serialEncode,
          serialEncodePure,
          serialDecode,
          serialDecodePure,
          dynamicDecodePure,
          dynamicEncodePure,
          Payload,
          DynamicPayload,
          PayloadLength,
          hPutPayload,
          hGetPayload,
          payloadLength,
          getPayloadType,
          getDynamicPayloadType,
          getPayloadContent,
          genericPut,
          genericGet) where

-- import Data.Binary (Binary,encode,decode,Put,Get,put,get,putWord8,getWord8)
-- import qualified Data.Serialize as Ser
import Data.Serialize (Serialize,encode,decode,Put,Get,put,get,putWord8,getWord8)

import Control.Monad (liftM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B (hPut,hGet,length,take)
import Control.Exception (evaluate)
import System.IO (Handle)
import Data.Typeable (typeOf,typeOf,Typeable)
import Data.Dynamic (Dynamic,toDyn,fromDynamic,dynTypeRep)
import Data.Generics (Data,gfoldl,gunfold, toConstr,constrRep,ConstrRep(..),repConstr,extQ,extR,dataTypeOf)

import Text.Printf

-- | Data that can be sent as a message must implement
-- this class. The class has no functions of its own,
-- but instead simply requires that the type implement
-- both 'Typeable' and 'Binary'. Typeable can usually
-- be derived automatically. Binary requires the put and get
-- functions, which can be easily implemented by hand,
-- or you can use the 'genericGet' and 'genericPut' flavors,
-- which will work automatically for types implementing
-- 'Data'.
class (Serialize a,Typeable a) => Serializable a
instance (Serialize a,Typeable a) => Serializable a

data Payload = Payload
                { 
                  payloadType :: !ByteString,
                  payloadContent :: !ByteString
                } deriving (Typeable)

instance Show Payload where
  show payload = "<type: "++ show (getPayloadType payload) ++", bytes: "
		 ++ show (B.take 100$ getPayloadContent payload) ++ ">"

data DynamicPayload = DynamicPayload
                {
                  dynamicPayloadContent :: Dynamic
                }
type PayloadLength = Int

instance Serialize Payload where
  put pl = put (payloadType pl) >> put (payloadContent pl)
  get = get >>= \a -> get >>= \b -> return $ Payload {payloadType = a,payloadContent=b}

payloadLength :: Payload -> PayloadLength
payloadLength (Payload t c) = B.length t + B.length c

getPayloadContent :: Payload -> ByteString
getPayloadContent = payloadContent

getPayloadType :: Payload -> String
getPayloadType pl = either (error "error decoding payload type")
                           Prelude.id
                           (decode $ payloadType pl)

hPutPayload :: Handle -> Payload -> IO ()
hPutPayload h (Payload t c) = B.hPut h (encode (B.length t :: PayloadLength)) >>  
                       B.hPut h t >>
                       B.hPut h (encode (B.length c :: PayloadLength)) >>
                       B.hPut h c

hGetPayload :: Handle -> IO Payload
hGetPayload h = do tl <- B.hGet h (fromIntegral baseLen)
                   let (Right tl') = decode tl
                   t <- B.hGet h (fromIntegral (tl' :: PayloadLength))
                   cl <- B.hGet h (fromIntegral baseLen)
                   let (Right cl') = decode cl
                   c <- B.hGet h (fromIntegral (cl' :: PayloadLength))
                   return $ Payload {payloadType = t,payloadContent = c}
    where baseLen = B.length (encode (0::PayloadLength))

serialEncodePure :: (Serializable a) => a -> Payload
serialEncodePure a = let encoding = encode a
                      in encoding `seq` Payload {payloadType = encode $ show $ typeOf a,
                                                 payloadContent = encoding}

dynamicEncodePure :: (Serializable a) => a -> DynamicPayload
dynamicEncodePure a = DynamicPayload {dynamicPayloadContent = toDyn a}

dynamicDecodePure :: (Serializable a) => DynamicPayload -> Maybe a
dynamicDecodePure a = fromDynamic (dynamicPayloadContent a)

getDynamicPayloadType :: DynamicPayload -> String
getDynamicPayloadType a = show (dynTypeRep (dynamicPayloadContent a))

-- TODO I suspect that we will get better performance for big messages if let this be lazy
-- see also serialDecode
serialEncode :: (Serializable a) => a -> IO Payload
serialEncode a = do encoded <- evaluate $ encode a -- this evaluate is actually necessary, it turns out; it might be better to just use strict ByteStrings
                    return $ Payload {payloadType = encode $ show $ typeOf a,
                                        payloadContent = encoded}


serialDecodePure :: forall a. (Serializable a) => Payload -> Maybe a
serialDecodePure p = 
  let pc = payloadContent p      
  in pc `seq`
       case decode $! payloadType p of
         Right str | str == show (typeOf $ (undefined :: a)) ->
           either (const Nothing) Just $! decode pc
         _ -> Nothing


serialDecode :: forall a. (Serializable a) => Payload -> IO (Maybe a)
serialDecode a = 
  case decode $ payloadType a of
    Right str | str == show (typeOf $ (undefined :: a)) -> do
      res <- evaluate $ decode (payloadContent a)
      case res of
        Left _  -> return $ Nothing
        Right v -> return . Just $ v
    _ -> return Nothing


-- | Data types that can be used in messaging must
-- be serializable, which means that they must implement
-- the 'get' and 'put' methods from 'Binary'. If you
-- are too lazy to write these functions yourself,
-- you can delegate responsibility to this function.
-- It's usually sufficient to do something like this:
--
-- > import Data.Data (Data)
-- > import Data.Typeable (Typeable)
-- > import Data.Binary (Binary, get, put)
-- > data MyType = MkMyType Foobar Int [(String, Waddle Baz)]
-- >             | MkSpatula
-- >                  deriving (Data, Typeable)
-- > instance Binary MyType where
-- >    put = genericPut
-- >    get = genericGet
genericPut :: (Data a) => a ->  Put
genericPut = generic `extQ` genericString
   where generic what = fst $ gfoldl 
            (\(before, a_to_b) a -> (before >> genericPut a, a_to_b a))
            (\x -> (serializeConstr (constrRep (toConstr what)), x))
            what
         genericString :: String -> Put
         genericString = put.encode

-- | This is the counterpart 'genericPut'
genericGet :: forall a. Data a => Get a
genericGet = generic `extR` genericString
   where generic = liftM id $ deserializeConstr $ \constr_rep ->
                     gunfold (\n -> do n' <- n
                                       g' <- genericGet
                                       return $ n' g')
                             return
                             (repConstr (dataTypeOf (undefined :: a)) constr_rep)
         genericString :: Get String
         genericString = do q <- get
                            either (error "genericString")
                                   return
                                   (decode q)

serializeConstr :: ConstrRep -> Put
serializeConstr (AlgConstr ix)   = putWord8 1 >> put ix
serializeConstr (IntConstr i)    = putWord8 2 >> put i
serializeConstr (FloatConstr r)  = putWord8 3 >> put r
#if __GLASGOW_HASKELL__ >= 611
serializeConstr (CharConstr c)   = putWord8 4 >> put c
#else
serializeConstr (StringConstr c)   = putWord8 4 >> put (head c)
#endif

deserializeConstr :: (ConstrRep -> Get a) -> Get a
deserializeConstr k = 
      do constr_ix <- getWord8
         case constr_ix of
          1 -> get >>= \ix -> k (AlgConstr ix)
          2 -> get >>= \i -> k (IntConstr i)
          3 -> get >>= \r -> k (FloatConstr r)
#if __GLASGOW_HASKELL__ >= 611
          4 -> get >>= \c -> k (CharConstr c)
#else
          4 -> get >>= \c -> k (StringConstr (c:[]))
#endif
          _ -> fail (printf "RPC.Encoding: invalid constr_ix %d" constr_ix)
