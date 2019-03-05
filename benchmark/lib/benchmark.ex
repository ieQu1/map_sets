# I don't know Elixir, so the below code probably doesn't make sense

l1 = Enum.to_list(1..10_000)
l2 = Enum.to_list(10_001..20_000)
l3 = Enum.to_list(5_000..15_000)

defmodule MapSets do
  def run(name, fun, sets0) do
    sets = Enum.map sets0, &:sets.from_list/1
    map_sets = Enum.map sets0, &:map_sets.from_list/1
    Benchee.run(%{ 'sets ' ++ name => fn -> :erlang.apply(:sets, fun, sets) end,
                   'map_sets ' ++ name => fn -> :erlang.apply(:map_sets, fun, map_sets) end
                 },
                 time: 10,
                 memory_time: 2,
                 formatters: [ Benchee.Formatters.HTML,
                               Benchee.Formatters.Console,
                               {Benchee.Formatters.CSV, file: name ++ '.csv'}
                             ]
               )
  end
end
MapSets.run('empty intersection', :intersection, [l1, l2])

MapSets.run('intersection', :intersection, [l1, l3])

MapSets.run('union', :union, [l1, l3])
