defmodule Eflame.Mixfile do
  use Mix.Project

  def project do
    [app: :eflame,
     language: :erlang,
     name: "eflame",
     description: "Flame Graph profiler for Erlang and Elixir.",
     version: "1.0.0",
     package: package(),
     deps: deps()]
  end

  defp deps do
    [{:ex_doc, ">= 0.0.0", only: :dev}]
  end

  defp package do
    [contributors: ["Vladimir Kirillov"],
     maintainers: ["Nebo #15"],
     licenses: ["LISENSE.md"],
     links: %{github: "https://github.com/proger/eflame"},
     files: ~w(src LICENSE mix.exs README.md stack_to_flame.sh stacks_to_flames.sh flamegraph.pl)]
  end
end
