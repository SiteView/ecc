package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class NfsServerV3Info extends Info {
	long getattr = 0L;

	long setattr = 0L;

	long lookup = 0L;

	long access = 0L;

	long readlink = 0L;

	long read = 0L;

	long write = 0L;

	long create = 0L;

	long mkdir = 0L;

	long symlink = 0L;

	long mknod = 0L;

	long remove = 0L;

	long rmdir = 0L;

	long rename = 0L;

	long link = 0L;

	long readdir = 0L;

	long readdirplus = 0L;

	long fsstat = 0L;

	long fsinfo = 0L;

	long pathconf = 0L;

	long commit = 0L;

	public long getGetattr() {
		return getattr;
	}

	public void setGetattr(long getattr) {
		this.getattr = getattr;
	}

	public long getSetattr() {
		return setattr;
	}

	public void setSetattr(long setattr) {
		this.setattr = setattr;
	}

	public long getLookup() {
		return lookup;
	}

	public void setLookup(long lookup) {
		this.lookup = lookup;
	}

	public long getAccess() {
		return access;
	}

	public void setAccess(long access) {
		this.access = access;
	}

	public long getReadlink() {
		return readlink;
	}

	public void setReadlink(long readlink) {
		this.readlink = readlink;
	}

	public long getRead() {
		return read;
	}

	public void setRead(long read) {
		this.read = read;
	}

	public long getWrite() {
		return write;
	}

	public void setWrite(long write) {
		this.write = write;
	}

	public long getCreate() {
		return create;
	}

	public void setCreate(long create) {
		this.create = create;
	}

	public long getMkdir() {
		return mkdir;
	}

	public void setMkdir(long mkdir) {
		this.mkdir = mkdir;
	}

	public long getSymlink() {
		return symlink;
	}

	public void setSymlink(long symlink) {
		this.symlink = symlink;
	}

	public long getMknod() {
		return mknod;
	}

	public void setMknod(long mknod) {
		this.mknod = mknod;
	}

	public long getRemove() {
		return remove;
	}

	public void setRemove(long remove) {
		this.remove = remove;
	}

	public long getRmdir() {
		return rmdir;
	}

	public void setRmdir(long rmdir) {
		this.rmdir = rmdir;
	}

	public long getRename() {
		return rename;
	}

	public void setRename(long rename) {
		this.rename = rename;
	}

	public long getLink() {
		return link;
	}

	public void setLink(long link) {
		this.link = link;
	}

	public long getReaddir() {
		return readdir;
	}

	public void setReaddir(long readdir) {
		this.readdir = readdir;
	}

	public long getReaddirplus() {
		return readdirplus;
	}

	public void setReaddirplus(long readdirplus) {
		this.readdirplus = readdirplus;
	}

	public long getFsstat() {
		return fsstat;
	}

	public void setFsstat(long fsstat) {
		this.fsstat = fsstat;
	}

	public long getFsinfo() {
		return fsinfo;
	}

	public void setFsinfo(long fsinfo) {
		this.fsinfo = fsinfo;
	}

	public long getPathconf() {
		return pathconf;
	}

	public void setPathconf(long pathconf) {
		this.pathconf = pathconf;
	}

	public long getCommit() {
		return commit;
	}

	public void setCommit(long commit) {
		this.commit = commit;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Getattr", getattr);
		map.put("Setattr", setattr);
		map.put("Lookup", lookup);
		map.put("Access", access);
		map.put("Readlink", readlink);
		map.put("Read", read);
		map.put("Write", write);
		map.put("Create", create);
		map.put("Mkdir", mkdir);
		map.put("Symlink", symlink);
		map.put("Mknod", mknod);
		map.put("Remove", remove);
		map.put("Rmdir", rmdir);
		map.put("Rename", rename);
		map.put("Link", link);
		map.put("Readdir", readdir);
		map.put("Readdirplus", readdirplus);
		map.put("Fsstat", fsstat);
		map.put("Fsinfo", fsinfo);
		map.put("Pathconf", pathconf);
		map.put("Commit", commit);
		return map;
	}
}
