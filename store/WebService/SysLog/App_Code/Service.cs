using System;
using System.Web;
using System.Web.Services;
using System.Web.Services.Protocols;
using Otp;

[WebService(Namespace = "http://tempuri.org/")]
[WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
public class Service : System.Web.Services.WebService
{

    public Service()
    {

        //如果使用设计的组件，请取消注释以下行 
        //InitializeComponent(); 
    }

    [WebMethod]
    public string getExtra_data(String Syslogid)
    {

        //kvs_output:get_extra_data(Syslogid)

        Otp.Erlang.Object[] args = new Otp.Erlang.Object[] { new Otp.Erlang.String(Syslogid) };

        Config.Connection.sendRPC("kvs_output", "get_extra_data", args);

        Otp.Erlang.Tuple result = null;
        result = (Otp.Erlang.Tuple)Config.Connection.receiveRPC();
        if (result.elementAt(0).ToString() == "ok")
        {
            return System.Text.Encoding.Default.GetString(((Otp.Erlang.Binary)result.elementAt(1)).binaryValue());
        }
        else
        {
            return string.Empty;
        }

    }

    [WebMethod]
    public string getRelationSyslog(String Syslogid, int num)
    {
        //kvs_output:get_relation_log(Syslogid, Num)

        Otp.Erlang.Object[] args = new Otp.Erlang.Object[] { new Otp.Erlang.String(Syslogid), new Otp.Erlang.Int(num) };

        Config.Connection.sendRPC("kvs_output", "get_relation_log", args);

        Otp.Erlang.Tuple result = null;
        result = (Otp.Erlang.Tuple)Config.Connection.receiveRPC();
        if (result.elementAt(0).ToString() == "ok")
        {
            return System.Text.Encoding.Default.GetString(((Otp.Erlang.Binary)result.elementAt(1)).binaryValue());
        }
        else
        {
            return string.Empty;
        }
    }

}
