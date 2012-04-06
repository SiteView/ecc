package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class NfsClientV2Info extends Info {
	long getattr = 0L;

	long setattr = 0L;

	long root = 0L;

	long lookup = 0L;

	long readlink = 0L;

	long read = 0L;

	long writecache = 0L;

	long write = 0L;

	long create = 0L;

	long remove = 0L;

	long rename = 0L;

	long link = 0L;

	long symlink = 0L;

	long mkdir = 0L;

	long rmdir = 0L;

	long readdir = 0L;

	long fsstat = 0L;

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

	public long getRoot() {
		return root;
	}

	public void setRoot(long root) {
		this.root = root;
	}

	public long getLookup() {
		return lookup;
	}

	public void setLookup(long lookup) {
		this.lookup = lookup;
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

	public long getWritecache() {
		return writecache;
	}

	public void setWritecache(long writecache) {
		this.writecache = writecache;
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

	public long getRemove() {
		return remove;
	}

	public void setRemove(long remove) {
		this.remove = remove;
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

	public long getSymlink() {
		return symlink;
	}

	public void setSymlink(long symlink) {
		this.symlink = symlink;
	}

	public long getMkdir() {
		return mkdir;
	}

	public void setMkdir(long mkdir) {
		this.mkdir = mkdir;
	}

	public long getRmdir() {
		return rmdir;
	}

	public void setRmdir(long rmdir) {
		this.rmdir = rmdir;
	}

	public long getReaddir() {
		return readdir;
	}

	public void setReaddir(long readdir) {
		this.readdir = readdir;
	}

	public long getFsstat() {
		return fsstat;
	}

	public void setFsstat(long fsstat) {
		this.fsstat = fsstat;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Getattr", getattr);
		map.put("Setattr", setattr);
		map.put("Root", root);
		map.put("Lookup", lookup);
		map.put("Readlink", readlink);
		map.put("Read", read);
		map.put("Writecache", writecache);
		map.put("Write", write);
		map.put("Create", create);
		map.put("Remove", remove);
		map.put("Rename", rename);
		map.put("Link", link);
		map.put("Symlink", symlink);
		map.put("Mkdir", mkdir);
		map.put("Rmdir", rmdir);
		map.put("Readdir", readdir);
		map.put("Fsstat", fsstat);
		return map;
	}
}
