defmodule Pantagruel.Values.Variable do
  @moduledoc """
  A bound value in an evaluated Pantagruel program, with a name and the
  domain it is in.
  """
  defstruct name: "", domain: ""
end
