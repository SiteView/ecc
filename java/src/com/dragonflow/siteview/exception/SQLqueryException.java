package com.dragonflow.siteview.exception;

import java.sql.SQLException;

public class SQLqueryException extends SQLException {


	public SQLqueryException(String reason) {
		super(reason);
	}

	private static final long serialVersionUID = 1L;

}
