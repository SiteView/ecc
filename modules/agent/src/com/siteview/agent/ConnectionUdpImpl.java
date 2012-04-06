package com.siteview.agent;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketException;

public class ConnectionUdpImpl extends Connection {

	private Log logger = Log.getInstance(ConnectionUdpImpl.class);

	private byte[] buffer = new byte[512];
	private DatagramSocket ds = null;
	private DatagramPacket packet;
	private String socketAddress;
	private int port;

	public ConnectionUdpImpl(String socketAddress, int port) {
		this.socketAddress = socketAddress;
		this.port = port;
	}

	@Override
	public void Close() {
		if (ds != null && !ds.isClosed())
			ds.close();
	}

	@Override
	public void Open() {
		InetSocketAddress socketAddress = new InetSocketAddress(
				this.socketAddress, this.port);
		try {
			ds = new DatagramSocket(socketAddress);
		} catch (SocketException e) {
			logger.error("Open", e);
		}
	}

	@Override
	public RequestBase Receive() {
		packet = new DatagramPacket(buffer, buffer.length);
		String requestStr = null;
		try {
			ds.receive(packet);
			requestStr = new String(packet.getData(), 0, packet.getLength());
		} catch (IOException e) {
			// 监听失败或者线程被中断
			logger.error("Receive", e);
		}

		return RequestFactory.instance(requestStr);
	}

	@Override
	public void SendBack(Response response) {
		DatagramPacket dp = new DatagramPacket(buffer, buffer.length, packet
				.getAddress(), packet.getPort());
		dp.setData(response.toJson().getBytes());
		try {
			ds.send(dp);
			logger.info("Response", response.toJson());
		} catch (IOException e) {
			logger.error("SendBack", e);
		}
	}

}
