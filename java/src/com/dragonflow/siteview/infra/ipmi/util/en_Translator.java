package com.dragonflow.siteview.infra.ipmi.util;

import com.dragonflow.siteview.infra.ipmi.commands.SensorInfoRecord;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;


public class en_Translator
    implements IPMITranslator
{

    public en_Translator()
    {
    }

    public String getSensorType(int id)
    {
        if(id > 41)
            return "reserved";
        if(id < 0)
        {
            return "Unsupported";
        } else
        {
            return SENSOR_TYPES_STRINGS[id];
        }
    }

    public String getUnitDescrition(int id)
    {
        return UNIT_DESCRIPTION_STRINGS[id];
    }

    public String getSensorStatus(int id)
    {
        if(id > 3)
            return "Unknown";
        else
            return SENSOR_STATUS_STRINGS[id];
    }

    public String getLastRestartCause(byte causeID)
    {
        if(causeID > 9)
            return "error!";
        else
            return LAST_RESTART_CAUSE_STRINGS[causeID];
    }

    public String getSensorAssert(SensorInfoRecord sensorInfoRecord, byte offset)
    {
        byte eventReadingTypeCode = sensorInfoRecord.getEventReadingTypeCodeByte();
        String strings[] = null;
        if(eventReadingTypeCode < 111)
            strings = (String[])GENERIC_SENSOR_ASSERTS_MAPPING.get(Byte.valueOf(eventReadingTypeCode));
        else
        if(eventReadingTypeCode <= 111)
            strings = (String[])SENSOR_SPECIFIC_ASSERTS_MAPPING.get(Byte.valueOf(sensorInfoRecord.getSensorType()));
        if(strings == null || offset > strings.length)
        {
            return "n/a";
        } else
        {
            return strings[offset];
        }
    }

    public String getGenericErrorMessage(byte ccode)
    {
        int index = ccode & 255;
        if(index >= 192 && index <= 213)
            return GENERIC_ERROR_CODES[index - 192 & 255];
        if(ccode <= 255)
            return "Unspecified error.";
        else
            return "";
    }

    public String getEntityNameById(byte id)
    {
        if(id > 208)
            return "reserved";
        else
            return ENTITY_TYPES_STRINGS[id];
    }

    private static final String SENSOR_TYPES_STRINGS[] = {
        "reserved", "Temperature", "Voltage", "Current", "Fan", "Physical Security (Chassis Intrusion)", "Platform Security Violation Attempt", "Processor", "Power Supply", "Power Unit", 
        "Cooling Device", "Other Units-based Sensor", "Memory", "Drive Slot(Bay)", "POST Memory Resize", "System Firmware Progress", "Event Logging Disabled", "Watchdog 1", "System Event", "Critical Interrupt", 
        "Button/Switch", "Module/Board", "Microcontroller/Coprocessor", "Add-in Card", "Chassis", "Chip Set", "Other FRU", "Cable/Interconnect", "Terminator", "System Boot Initiated", 
        "Boot Error", "OS Boot", "OS Critical Stop", "Slot/Connector", "System ACPI Power State", "Watchdog 2", "Platform Alert", "Entity Presence", "Monitor ASIC/IC", "LAN", 
        "Management Subsystem Health", "Battery"
    };
    private static final String ENTITY_TYPES_STRINGS[] = {
        "Unspecified", "Other", "Unknown", "Processor", "Disk or Disk Bay", "Peripheral bay", "System Management Module", "System Board", "Memory Module", "Processor Module", 
        "Power Supply", "Add-in Card", "Front Panel Board", "Back Panel Board", "Power System Board", "Drive Backplane", "System Internal Expansion Board", "Other System Board", "Processor Board", "Power Unit / Power Domain", 
        "Power Module / DC-to-DC Converter", "Power Management / Power Distribution board", "Chassis Back Panel Board", "System chassis", "Sub-chassis", "Other Chassis Board", "Disk Drive Bay", "Peripheral Bay", "Device Bay", "Fan / Cooling Device", 
        "Cooling Unit", "Cable / Interconnect", "Memory Device", "System Management Software", "BIOS", "Operating System", "System Bus", "Group", "Remote (Out of Band) Management Communication Device", "External Environment", 
        "Battery", "Chassis-specific Entities", "Board-set specific Entities", "OEM System Integrator Defined"
    };
    private static final String UNIT_DESCRIPTION_STRINGS[] = {
        "unspecified", "degrees C", "degrees F", "degrees K", "Volts", "Amps", "Watts", "Joules", "Coulombs", "VA", 
        "Nits", "lumen", "lux", "Candela", "kPa", "PSI", "Newton", "CFM", "RPM", "Hz", 
        "microsecond", "millisecond", "second", "minute", "hour", "day", "week", "mil", "inches", "feet", 
        "cu in", "cu feet", "mm", "cm", "m", "cu cm", "cu m", "liters", "fluid ounce", "radians", 
        "steradians", "revolutions", "cycles", "gravities", "ounce", "pound", "ft-lb", "oz-in", "gauss", "gilberts", 
        "henry", "millihenry", "farad", "microfarad", "ohms", "siemens", "mole", "becquerel", "PPM", "reserved", 
        "Decibels", "DbA", "DbC", "gray", "sievert", "color temp deg K", "bit", "kilobit", "megabit", "gigabit", 
        "byte", "kilobyte", "megabyte", "gigabyte", "word", "dword", "qword", "line", "hit", "miss", 
        "retry", "reset", "overflow", "underrun", "collision", "packets", "messages", "characters", "error", "correctable error", 
        "uncorrectable error"
    };
    private static final String SENSOR_STATUS_STRINGS[] = {
        "OK", "Warning", "Error", "Unknown"
    };
    private static final String LAST_RESTART_CAUSE_STRINGS[] = {
        "unknown", "chassis power control command", "reset via pushbutton", "power-up via pushbutton", "watchdog expired", "OEM", "power-up due to always-restore power policy", "power-up due to restore-previous power policy", "reset via PEF", "power-cycle via PEF"
    };
    private static final ConcurrentMap GENERIC_SENSOR_ASSERTS_MAPPING;
    private static final ConcurrentMap SENSOR_SPECIFIC_ASSERTS_MAPPING;
    private static final String GENERIC_ERROR_CODES[] = {
        "Node Busy. Command could not be processed because command processing resources are temporarily unavailable.", "Invalid Command. Used to indicate an unrecognized or unsupported command.", "Command invalid for given LUN.", "Timeout while processing command. Response unavailable.", "Out of space. Command could not be completed because of a lack of storage space required to execute the given command operation.", "Reservation Canceled or Invalid Reservation ID.", "Request data truncated.", "Request data length invalid.", "Request data field length limit exceeded.", "Parameter out of range. One or more parameters in the data field of the Request are out of range. This is different from \uFFFDInvalid data field\uFFFD (CCh) code in that it indicates that the erroneous field(s) has a contiguous range of possible values.", 
        "Cannot return number of requested data bytes.", "Requested Sensor, data, or record not present.", "Invalid data field in Request", "Command illegal for specified sensor or record type.", "Command response could not be provided.", "Cannot execute duplicated request. This completion code is for devices which cannot return the response that was returned for the original instance of the request. Such devices should provide separate commands that allow the completion status of the original request to be determined. An Event Receiver does not use this completion code, but returns the 00h completion code in the response to (valid) duplicated requests.", "Command response could not be provided. SDR Repository in update mode.", "Command response could not be provided. Device in firmware update mode.", "Command response could not be provided. BMC initialization or initialization agent in progress.", "Destination unavailable. Cannot deliver request to selected destination. E.g. this code can be returned if a request message is targeted to SMS, but receive message queue reception is disabled for the particular channel.", 
        "Cannot execute command. Insufficient privilege level.", "Cannot execute command. Command, or request parameter(s), not supported in present state.", "Unspecified error."
    };

    static 
    {
        GENERIC_SENSOR_ASSERTS_MAPPING = new ConcurrentHashMap();
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)1), new String[] {
            "Lower Non-critical - going low", "Lower Non-critical - going high", "Lower Critical - going low", "Lower Critical - going high", "Lower Non-recoverable - going low", "Lower Non-recoverable - going high", "Upper Non-critical - going low", "Upper Non-critical - going high", "Upper Critical - going low", "Upper Critical - going high", 
            "Upper Non-recoverable - going low", "Upper Non-recoverable - going high"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)2), new String[] {
            "Transition to Idle", "Transition to Active", "Transition to Busy"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)3), new String[] {
            "State Deasserted", "State Asserted"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)4), new String[] {
            "Predictive Failure deasserted", "Predictive Failure asserted"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)5), new String[] {
            "Limit Not Exceeded", "Limit Exceeded"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)6), new String[] {
            "Performance Met", "Performance Lags"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)7), new String[] {
            "transition to OK", "transition to Non-Critical from OK", "transition to Critical from less severe", "transition to Non-recoverable from less severe", "transition to Non-Critical from more severe", "transition to Critical from Non-recoverable", "transition to Non-recoverable", "Monitor", "Informational"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)8), new String[] {
            "Device Removed / Device Absent", "Device Inserted / Device Present"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)9), new String[] {
            "Device Disabled", "Device Enabled"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)10), new String[] {
            "transition to Running", "transition to In Test", "transition to Power Off", "transition to On Line", "transition to Off Line", "transition to Off Duty", "transition to Degraded", "transition to Power Save", "Install Error"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)11), new String[] {
            "Fully Redundant", "Redundancy Lost", "Redundancy Degraded", "Non-redundant", "Non-redundant", "Non-redundant", "Redundancy Degraded", "Redundancy Degraded"
        });
        GENERIC_SENSOR_ASSERTS_MAPPING.put(new Byte((byte)12), new String[] {
            "D0 Power State", "D1 Power State", "D2 Power State", "D3 Power State"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING = new ConcurrentHashMap();
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)5), new String[] {
            "General Chassis Intrusion", "Drive Bay intrusion", "I/O Card area intrusion", "Processor area intrusion", "LAN Leash Lost (system is unplugged from LAN)", "Unauthorized dock/undock", "FAN area intrusion"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)6), new String[] {
            "Secure Mode (Front Panel Lockout) Violation attempt", "Pre-boot Password Violation - user password", "Pre-boot Password Violation attempt - setup password", "Pre-boot Password Violation - network boot password", "Other pre-boot Password Violation", "Out-of-band Access Password Violation"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)7), new String[] {
            "IERR", "Thermal Trip", "FRB1/BIST failure", "FRB2/Hang in POST failure", "FRB3/Processor Startup/Initialization failure", "Configuration Error", "SM BIOS 'Uncorrectable CPU-complex Error'", "Processor Presence detected", "Processor disabled", "Terminator Presence Detected"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)8), new String[] {
            "Presence detected", "Power Supply Failure detected", "Predictive Failure", "Power Supply AC lost", "AC lost or out-of-range", "AC out-of-range, but present"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)9), new String[] {
            "Power Off / Power Down", "Power Cycle", "240VA Power Down", "Interlock Power Down", "AC lost", "Soft Power Control Failure", "Power Unit Failure detected", "Predictive Failure"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)12), new String[] {
            "Correctable ECC / other correctable memory error", "Uncorrectable ECC / other uncorrectable memory error", "Parity", "Memory Scrub Failed (stuck bit)", "Memory Device Disabled", "Correctable ECC / other correctable memory error logging limit reached"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)15), new String[] {
            "System Firmware Error (POST Error)", "System Firmware Hang (uses same Event Data 2 definition as following System Firmware Progress offset)", "System Firmware Progress"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)16), new String[] {
            "Correctable Memory Error Logging Disabled", "Event \uFFFDType\uFFFD Logging Disabled. Event Logging is disabled for following event/reading type and offset has been disabled.", "Log Area Reset/Cleared", "All Event Logging Disabled"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)17), new String[] {
            "BIOS Watchdog Reset", "OS Watchdog Reset", "OS Watchdog Shut Down", "OS Watchdog Power Down", "OS Watchdog Power Cycle", "OS Watchdog NMI / Diagnostic Interrupt", "OS Watchdog Expired, status only", "OS Watchdog pre-timeout Interrupt, non-NMI"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)18), new String[] {
            "System Reconfigured", "OEM System Boot Event", "Undetermined system hardware failure", "Entry added to Auxiliary Log", "PEF Action"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)19), new String[] {
            "Front Panel NMI / Diagnostic Interrupt", "Bus Timeout", "I/O channel check NMI", "Software NMI", "PCI PERR", "PCI SERR", "EISA Fail Safe Timeout", "Bus Correctable Error", "Bus Uncorrectable Error", "Fatal NMI (port 61h, bit 7)"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)20), new String[] {
            "Power Button pressed", "Sleep Button pressed", "Reset Button pressed"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)29), new String[] {
            "Initiated by power up", "Initiated by hard reset", "Initiated by warm reset", "User requested PXE boot", "Automatic boot to diagnostic"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)30), new String[] {
            "No bootable media", "Non-bootable diskette left in drive", "PXE Server not found", "Invalid boot sector", "Timeout waiting for user selection of boot source"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)31), new String[] {
            "A: boot completed", "C: boot completed", "PXE boot completed", "Diagnostic boot completed", "CD-ROM boot completed", "ROM boot completed", "boot completed - boot device not specified"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)32), new String[] {
            "Stop during OS load / initialization", "Run-time Stop"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)33), new String[] {
            "Fault Status asserted", "Identify Status asserted", "Slot / Connector Device installed/attached", "Slot / Connector Ready for Device Installation", "Slot/Connector Ready for Device Removal", "Slot Power is Off", "Slot / Connector Device Removal Request", "Interlock asserted", "Slot is Disabled"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)34), new String[] {
            "S0 / G0 working", "S1 sleeping with system h/w & processor context maintained", "S2 sleeping, processor context lost", "S3 sleeping, processor & h/w context lost, memory retained.", "S4 non-volatile sleep / suspend-to disk", "S5 / G2 soft-off", "S4 / S5 soft-off, particular S4 / S5 state cannot be determined", "G3 / Mechanical Off", "Sleeping in an S1, S2, or S3 states", "G1 sleeping (S1-S4 state cannot be determined)", 
            "S5 entered by override", "Legacy ON state", "Legacy OFF state", "Unknown"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)35), new String[] {
            "Timer expired, status only (no action, no interrupt)", "Hard Reset", "Power Down", "Power Cycle", "n/a", "n/a", "n/a", "n/a", "Timer interrupt"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)36), new String[] {
            "platform generated page", "platform generated LAN alert", "Platform Event Trap generated, formatted per IPMI PET specification", "platform generated SNMP trap, OEM format"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)37), new String[] {
            "Entity Present", "Entity Absent", "Entity Disabled"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)39), new String[] {
            "LAN Heartbeat Lost", "LAN Heartbeat"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)40), new String[] {
            "sensor access degraded or unavailable", "controller access degraded or unavailable", "management controller off-line", "management controller unavailable"
        });
        SENSOR_SPECIFIC_ASSERTS_MAPPING.put(new Byte((byte)41), new String[] {
            "battery low (predictive failure)", "battery failed", "battery presence detected"
        });
    }
}
