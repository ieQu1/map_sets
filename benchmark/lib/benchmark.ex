# I don't know Elixir, so the below code probably doesn't make sense

defmodule MapSets do
  def make_random_list(n, m) do
    Enum.shuffle (for i <- 1..n, j <- 1..m, do: i)
  end

  def make_inputs(n) do
    { Enum.shuffle(Enum.to_list(1 .. n)),
      Enum.shuffle(Enum.to_list(div(n, 2) .. div(n*3, 2))),
      Enum.shuffle(Enum.to_list(n + 1 .. 2*n + 1))
    }
  end

  def make_sets({a, b, c}) do
    { :sets.from_list(a),
      :sets.from_list(b),
      :sets.from_list(c)
    }
  end

  def make_map_sets({a, b, c}) do
    { :map_sets.from_list(a),
      :map_sets.from_list(b),
      :map_sets.from_list(c)
    }
  end

  def make_gb_sets({a, b, c}) do
    { :gb_sets.from_list(a),
      :gb_sets.from_list(b),
      :gb_sets.from_list(c)
    }
  end

  def make_ordsets({a, b, c}) do
    { :ordsets.from_list(a),
      :ordsets.from_list(b),
      :ordsets.from_list(c)
    }
  end

  def inputs do
    %{
      "Small (100)" => make_inputs(100),
      "Middle (100 Thousand)" => make_inputs(100_000),
      "Big (1 Million)" => make_inputs(1_000_000),
    }
  end

  def run(name, fun) do
    Benchee.run(%{ name ++ ' map_sets' =>
                   { fn(input) -> fun.(:map_sets, input) end,
                     before_scenario: &make_map_sets/1
                   },
                   name ++ ' sets' =>
                   { fn(input) -> fun.(:sets, input) end,
                     before_scenario: &make_sets/1
                   },
                   name ++ ' gb_sets' =>
                   { fn(input) -> fun.(:gb_sets, input) end,
                     before_scenario: &make_gb_sets/1
                   },
                   name ++ ' ordsets' =>
                   { fn(input) -> fun.(:ordsets, input) end,
                     before_scenario: &make_ordsets/1
                   }
                 },
                 inputs: inputs(),
                 time: 5,
                 memory_time: 2,
                 formatters: [ {Benchee.Formatters.HTML, file: name ++ '.html', auto_open: false},
                               Benchee.Formatters.Console
                             ]
               )
  end
end

Benchee.run(%{'from_list sets' => &:sets.from_list/1,
              'from_list map_sets' => &:map_sets.from_list/1,
              'from_list gb_sets' => &:gb_sets.from_list/1,
              'from_list ordsets' => &:ordsets.from_list/1,
             },
            inputs: %{ "Small (100)" => MapSets.make_random_list(100, 10),
                       "Middle (100 Thousand)" => MapSets.make_random_list(100_000, 10),
                       "Large (1 Million)" => MapSets.make_random_list(1_000_000, 10),
                     },
            time: 5,
            memory_time: 2,
            formatters: [ {Benchee.Formatters.HTML, file: "from_list.html", auto_open: false},
                          Benchee.Formatters.Console,
                        ]
           )

MapSets.run(
  'is_element (true)',
  fn(module, {a, b, c}) -> module.is_element(100, a) end
)

MapSets.run(
  'is_element (false)',
  fn(module, {a, b, c}) -> module.is_element(:a, a) end
)

MapSets.run(
  'is_disjoint (true)',
  fn(module, {a, b, c}) -> module.is_disjoint(a, c) end
)

MapSets.run(
  'is_disjoint (false)',
  fn(module, {a, b, c}) -> module.is_disjoint(a, b) end
)

MapSets.run(
  'intersection (disjoint)',
  fn(module, {a, b, c}) -> module.intersection(a, c) end
)

MapSets.run(
  'intersection',
  fn(module, {a, b, c}) -> module.intersection(a, b) end
)

MapSets.run(
  'union',
  fn(module, {a, b, c}) -> module.union(a, b) end
)
