def run_1(TestName, {F1, F2}, M1) do
  MSN = TestName ++ " map_sets"
  SN  = TestName ++ " sets"
  Benchee.run %{
    MSN =>
    {
      fn({input, resource}) -> F1(input, resource) end,
      before_scenario: fn({input, resource}) ->
        resource = alter_resource(resource)
        {input, resource}
      end
    }
  }
end
