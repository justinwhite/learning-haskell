-- data types
--

type Sid = Int
type Timestamp = Integer

data Record = Record { sid :: Sid
			, ts :: Timestamp
			, price_per_share :: Float
			, number_shares :: Int
			} deriving (Eq, Show)

data Aggregate = Aggregate { volume :: Int, price :: Float }

-- generating the calls
--

fts :: Timestamp -> Timestamp -> Int -> Timestamp
fts start stop i = start + (toInteger i `mod` (stop - start))

fp :: Int -> Float
fp i = fromIntegral (50 + (i `mod` 10))

generate_calls :: Int -> Timestamp -> Timestamp -> [Record]
generate_calls n start stop = [ Record i ts price i | i <- [1..n], let price = fp i; ts = fts start stop i ]

-- pricing the records
--

vwap :: Aggregate -> Record -> Aggregate 
vwap (Aggregate v p) (Record _ ts pps ns) = Aggregate (v + ns) (p + (pps * (fromIntegral ns)))

between_timestamps :: Timestamp -> Timestamp -> Record -> Bool
between_timestamps t1 t2 r
	| ts r < t1 = False
	| ts r > t2 = False
	| otherwise = True

double_timestamp_aggregation :: Timestamp -> Timestamp -> [Record] -> Float
double_timestamp_aggregation start stop records = (price res) / (fromIntegral (volume res))
	where res = foldl vwap (Aggregate 0 0) (filter (between_timestamps start stop) records)

-- run a sample vwap
--

t1 = 1036903948 
t2 = 1436903948
start = 1036903948
stop = 1036908948
records = generate_calls 10000 t1 t2
result = double_timestamp_aggregation start stop records
count = length (filter (between_timestamps start stop) records)
