package com.dragonflow.siteview.infra.vmware;

import com.vmware.vim.ManagedObjectReference;

public class NamedManagedObject
    implements Comparable
{

    public NamedManagedObject(ManagedObjectReference obj, String name, String sortName)
    {
        this.obj = obj;
        this.name = name;
        this.sortName = sortName;
    }

    public int compareTo(NamedManagedObject o)
    {
        return sortName.compareTo(o.sortName);
    }

    public int compareTo(Object x0)
    {
        return compareTo((NamedManagedObject)x0);
    }

    public final ManagedObjectReference obj;
    public final String name;
    public final String sortName;
}

