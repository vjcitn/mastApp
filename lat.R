data(woncan)
mytab = woncan |> group_by(MSA) |> 
    summarize(lat=head(lat)[1], lng=head(lng)[1])
