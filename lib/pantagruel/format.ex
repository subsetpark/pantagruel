defmodule Pantagruel.Format.Formatter do
  alias Pantagruel.Env
  @type ast :: [term]
  @type t :: String.t()
  @callback format_program({:program, [any, ...]}) :: t
  @callback format_env(Env.t()) :: t()
end

defmodule Pantagruel.Format do
  @default Pantagruel.Format.Markdown
  def with_formatter(flags) do
    Keyword.update(
      flags,
      :formatter,
      @default,
      fn
        :markdown -> @default
      end
    )
  end
end
