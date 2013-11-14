defmodule SimpleEcto.Dynamo do
  use Dynamo
  use Dynamo.Router

  config :server, port: 3030

  templates do
    use Dynamo.Helpers
  end

  get "/" do
    conn = conn.assign(:title, "Average Weather by City")
    render conn, "index.html", results: WeatherQueries.all
  end
end

defmodule SimpleEcto do
  use Application.Behaviour

  @doc """
  The application callback used to start this
  application and its Dynamos.
  """
  def start(_type, _args) do
    SimpleEcto.Sup.start_link
  end
end

defmodule SimpleEcto.Sup do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({ :local, __MODULE__ }, __MODULE__, [])
  end

  def init([]) do
    tree = [ worker(SimpleEcto.Dynamo, []), worker(Repo, []) ]
    supervise(tree, strategy: :one_for_all)
  end
end

defmodule Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  # url ecto://username:password@hostname/database
  def url, do: "ecto://postgres@localhost/simple_ecto_db"
end

defmodule Weather do
  use Ecto.Model

  queryable "weather" do
    field :city, :string
    field :temp_lo, :integer
    field :temp_hi, :integer
    field :prcp, :float, default: 0.0
  end
end

defmodule WeatherQueries do
  import Ecto.Query

  def all do
    Enum.map(_all, fn(x) -> {x.id, x.city, x.temp_lo, x.temp_hi, x.prcp} end)
  end

  defp _all do
    query = from w in Weather,
            where: w.prcp > 0 or w.prcp == nil,
            select: w
    Repo.all(query)
  end
end
