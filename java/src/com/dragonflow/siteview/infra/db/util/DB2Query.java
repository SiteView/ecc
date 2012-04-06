package com.dragonflow.siteview.infra.db.util;

import java.util.Iterator;
import java.util.Set;

public class DB2Query
    implements Comparable
{

    public DB2Query(String name, String sql, String tableIndex, String description)
    {
        this.name = name;
        this.description = description;
        this.tableIndex = tableIndex;
        this.sql = sql;
    }

    public static DB2Query findQuery(Set queries, String name)
    {
        for(Iterator iterator = queries.iterator(); iterator.hasNext();)
        {
            DB2Query db2Query = (DB2Query)iterator.next();
            if(db2Query.name.equals(name))
                return db2Query;
        }

        return null;
    }

    public int hashCode()
    {
        return (new StringBuilder()).append(name).append(description).append(tableIndex).append(sql).toString().hashCode();
    }

    public int compareTo(Object o)
    {
        return name.compareTo(((DB2Query)o).name);
    }

    public String name;
    public String description;
    public String tableIndex;
    public String sql;
}

