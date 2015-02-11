facCPS :: a -> (a -> r) -> r
facCPS 0 k = k 1
facCPS n'@(n + 1) k = facCPS n $ \ret -> k (n' * ret)
