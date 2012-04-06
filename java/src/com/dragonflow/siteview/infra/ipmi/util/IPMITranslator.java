package com.dragonflow.siteview.infra.ipmi.util;

import com.dragonflow.siteview.infra.ipmi.commands.SensorInfoRecord;

public interface IPMITranslator
{

    public abstract String getSensorType(int i);

    public abstract String getUnitDescrition(int i);

    public abstract String getSensorStatus(int i);

    public abstract String getLastRestartCause(byte byte0);

    public abstract String getSensorAssert(SensorInfoRecord sensorinforecord, byte byte0);

    public abstract String getGenericErrorMessage(byte byte0);

    public abstract String getEntityNameById(byte byte0);
}
