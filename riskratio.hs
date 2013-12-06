{-
  summary :
    odds比を計算
  detail :
    与えられた群Aと、(全体-群A)との発生率との比を計算
  args :
    total          --全体の数
    incidence      --全体の発生数
    a_total        --群Aの全体数
    a_incidence    --群Aの発生数
-}
odd_ratio  total incidence a_total a_incidence =
  (a_incidence / a_total) / (incidence_exclude_a / total_exclude_a)
  where
      total_exclude_a = total - a_total
      incidence_exclude_a = incidence - a_incidence




{-
  summary :
    適合度検定(カイ2乗検定)の計算
  detail :
    与えられた群Aと、(全体-群A)との発生数から適合度検定を実施
  args :
    total         --全体の数
    incidence     --全体の発生数
    a_total       --群Aの全体数
    a_incidence   --群Aの発生数
-}
chisq total incidence a_total a_incidence =
  chisq' a_incidence estimate_incidence
  + chisq' a_no_incidence estimate_no_incidence
  where
    rate                  = (incidence - a_incidence) / (total - a_total) --(全体-群A)の発生率
    estimate_incidence    = a_total * rate                                --a群の予想発生数
    estimate_no_incidence = a_total * (1 - rate)                          --a群の予想非発生数
    a_no_incidence        = a_total - a_incidence                         --a群の非発生数
    chisq' actual estimate = (actual - estimate)**2 / estimate


fac :: Int -> Int
fac number
  | number > 1  =  number * fac  (number - 1)
  | number == 1 =  number
  | number == 0 = 1
  | otherwise = 0

fac2 :: Int -> Int
fac2 0 = 1
fac2 x = x * fac2(x - 1)
