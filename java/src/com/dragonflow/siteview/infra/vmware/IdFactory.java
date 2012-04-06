package com.dragonflow.siteview.infra.vmware;

import com.vmware.vim.DynamicProperty;
import com.vmware.vim.ManagedObjectReference;
import java.rmi.RemoteException;
import java.util.HashMap;

class IdFactory
{
    static interface Creator
    {

        public abstract Id create(String s);
    }

    private static class InvPathId
        implements Id
    {

        public int hashCode()
        {
            return 127 + (id != null ? id.hashCode() : 0);
        }

        public boolean equals(Object obj)
        {
            if(this == obj)
                return true;
            if(obj == null)
                return false;
            if(getClass() != obj.getClass())
                return false;
            InvPathId other = (InvPathId)obj;
            if(id == null)
            {
                if(other.id != null)
                    return false;
            } else
            if(!id.equals(other.id))
                return false;
            return true;
        }

        public String stringId()
        {
            return (new StringBuilder()).append("invpath").append(IdFactory.PREFIX_DELIMETER).append(id).toString();
        }

        public ManagedObjectReference getObject(Connector conn)
            throws RemoteException
        {
            ManagedObjectReference objectReference = conn.findByInventoryPath(id);
            return objectReference;
        }

        private static Creator getCreator()
        {
            return new Creator() {

                public Id create(String id)
                {
                    return new InvPathId(id);
                }

            }
;
        }

        static final String PREFIX = "invpath";
        private final String id;


        private InvPathId(String id)
        {
            this.id = id;
        }

    }

    private static class UUId
        implements Id
    {

        public String stringId()
        {
            return (new StringBuilder()).append("uuid").append(IdFactory.PREFIX_DELIMETER).append(id).toString();
        }

        public ManagedObjectReference getObject(Connector conn)
            throws RemoteException
        {
            ManagedObjectReference objectReference = conn.findByUUID(null, id, true);
            return objectReference;
        }

        private static Creator getCreator()
        {
            return new Creator() {

                public Id create(String id)
                {
                    return new UUId(id);
                }

            }
;
        }

        public int hashCode()
        {
            return 63 + (id != null ? id.hashCode() : 0);
        }

        public boolean equals(Object obj)
        {
            if(this == obj)
                return true;
            if(obj == null)
                return false;
            if(getClass() != obj.getClass())
                return false;
            UUId other = (UUId)obj;
            if(id == null)
            {
                if(other.id != null)
                    return false;
            } else
            if(!id.equals(other.id))
                return false;
            return true;
        }

        static final String PREFIX = "uuid";
        private String id;


        private UUId(String id)
        {
            this.id = id;
        }

    }


    IdFactory(Connector conn)
    {
        prefixResolutions = new HashMap();
        prefixResolutions.put("uuid", UUId.getCreator());
        prefixResolutions.put("invpath", InvPathId.getCreator());
        connector = conn;
        clean();
    }

    void clean()
    {
        invPathHash.clear();
    }

    public Id getId(String idStr)
    {
        int delim = idStr.indexOf(PREFIX_DELIMETER);
        String prefix = idStr.substring(0, delim);
        String id = idStr.substring(delim + 1);
        Creator creator = (Creator)prefixResolutions.get(prefix);
        Id idObj = creator.create(id);
        return idObj;
    }

    public Id getId(ManagedObjectReference objRef)
        throws RemoteException
    {
        if("VirtualMachine".equals(objRef.getType()))
        {
            String uuid = buildUUID(objRef);
            if(uuid != null)
                return new UUId(uuid);
        } else
        {
            String invpath = buildInventoryPath(objRef);
            if(invpath != null)
                return new InvPathId(invpath);
        }
        return null;
    }

    private String buildUUID(ManagedObjectReference objRef)
        throws RemoteException
    {
        String uuid = (String)uuidHash.get(objRef);
        if(uuid == null)
        {
            uuid = (String)connector.getVMProperty(objRef, "config.uuid");
            if(uuid != null)
                uuidHash.put(objRef, uuid);
        }
        return uuid;
    }

    private String buildInventoryPath(ManagedObjectReference obj)
        throws RemoteException
    {
        String path = (String)invPathHash.get(obj);
        if(path == null)
        {
            DynamicProperty props[] = connector.getVMProperty(obj, new String[] {
                "name", "parent"
            });
            if(props != null)
            {
                path = (String)props[0].getVal();
                ManagedObjectReference parent = props.length <= 1 ? null : (ManagedObjectReference)props[1].getVal();
                if(parent != null)
                    path = (new StringBuilder()).append(buildInventoryPath(parent)).append("/").append(path).toString();
                else
                    path = "";
                invPathHash.put(obj, path);
            }
        }
        return path;
    }

    private static String PREFIX_DELIMETER = ":";
    private HashMap prefixResolutions;
    Connector connector;
    private final HashMap invPathHash = new HashMap();
    private final HashMap uuidHash = new HashMap();


}
