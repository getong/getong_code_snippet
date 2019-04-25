defmodule FsWatchAdapter do
  use GenServer

  def start_link(dir) do
    GenServer.start_link(__MODULE__, dir, name: __MODULE__)
  end

  def init(dir) do
    state = %{
      port: nil,
      dir: dir
    }
    {:ok, state, {:continue, :start_fswatch}}
  end

  def handle_continue(:start_fswatch, state = %{dir: dir}) do
    cmd = "/usr/local/fswatch-1.14.0/bin/fswatch #{dir}"
    port = Port.open({:spawn, cmd}, [:binary, :exit_status])
    state = Map.put(state, :port, port)
    {:noreply, state}
  end

  def handle_info({port, {:data, msg}}, %{port: port} = state) do

    IO.puts "Received message from port: #{msg}, #{String.length(msg)}"
    {:noreply, state}
  end

  def handle_info({port, {:exit_status, _ext_status}}, %{port: port} = state) do
    {:stop, :normal, state}
  end
end
