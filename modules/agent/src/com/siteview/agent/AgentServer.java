package com.siteview.agent;

public abstract class AgentServer extends Thread {

	protected Connection connection;
	protected Dispatcher dispatcher;
	private boolean isInterrupted = false;

	public void run() {
		connection.Open();
		while (!isInterrupted) {
			RequestBase request = this.connection.Receive();
			if(isInterrupted)
				return;
			Response response = dispatcher.ProcessRequest(request);
			this.connection.SendBack(response);
		}
	}

	public void interrupt() {
		isInterrupted = true;
		this.connection.Close();
		super.interrupt();
	}

	public void Stop() {
		this.interrupt();
	}
}
