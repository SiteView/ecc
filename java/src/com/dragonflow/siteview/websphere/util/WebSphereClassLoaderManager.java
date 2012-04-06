package com.dragonflow.siteview.websphere.util;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class WebSphereClassLoaderManager {
	// public static final Log logger = LogFactory
	// .getEasyLog(WebSphereClassLoaderManager.class);

	public static final WebSphereClassLoaderManager INSTANCE = new WebSphereClassLoaderManager();
	private Map<Integer, ClassLoader> classLoaders = Collections
			.synchronizedMap(new HashMap());

	public static final WebSphereClassLoaderManager getInstance() {
		return INSTANCE;
	}

	public ClassLoader getClassLoader(
			WebSphereConnectorProperties connectorProperties) throws Exception {
		// if (logger.isDebugEnabled()) {
		// logger.debug("Entering getClassLoader() with  connectorProperties="
		// + connectorProperties);
		// }

		ClassLoader classLoader = null;

		synchronized (this) {
			classLoader = (ClassLoader) this.classLoaders.get(Integer
					.valueOf(connectorProperties.getHashKey()));

			if (classLoader == null) {
				// if (logger.isDebugEnabled()) {
				// logger.debug("Instantiating new ClassLoader["
				// + connectorProperties.getHashKey()
				// + "] because none was found in the cache");
				// }
				try {
					classLoader = createClassLoader(connectorProperties
							.getClassPath());
				} catch (IOException e) {
					throw new Exception("", e);
				} catch (ClasspathResourcesException e) {
					throw new Exception("", e);
				}
				this.classLoaders.put(Integer.valueOf(connectorProperties
						.getHashKey()), classLoader);
				// } else if (logger.isDebugEnabled()) {
				// logger
				// .debug("getClassLoader() got cached WebSphere ClassLoader="
				// + classLoader
				// + " with connectorProps.getHashKey()="
				// + connectorProperties.getHashKey());
			}

		}

		// if (logger.isDebugEnabled()) {
		// logger.debug("Leaving getClassLoader()");
		// }
		return classLoader;
	}

	public static ClassLoader createClassLoader(String classPath)
			throws IOException, ClasspathResourcesException {
//		if (logger.isDebugEnabled()) {
//			logger.debug("Entering addClassLoader() with Classpath: "
//					+ classPath);
//		}

		String[] resources = classPath.split(";");

		URL[] urlArray = new URL[resources.length];

		for (int i = 0; i < resources.length; ++i) {
			try {
				urlArray[i] = new File(resources[i]).toURI().toURL();
			} catch (MalformedURLException e) {
//				logger.error(
//						"WebSphereMonitor received MalformedURLException in getBrowseData(): "
//								+ e.getMessage(), e);
			}

		}

		ClassLoader classLoader = new URLClassLoader(urlArray);

//		if (logger.isDebugEnabled()) {
//			logger.debug("Leaving addClassLoader()");
//		}

		return classLoader;
	}
}