using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Otp;

/// <summary>
/// Config 的摘要说明
/// </summary>
public class Config
{

    public static readonly Otp.Erlang.Atom ATOM_OK = new Otp.Erlang.Atom("ok");
    private static Config config = new Config();

    private OtpConnection connection = null;

    public Config()
    {
        //
        // TODO: 在此处添加构造函数逻辑
        //

        if (connection == null)
        {

            string server = null;
            server = System.Configuration.ConfigurationManager.AppSettings["server"];
            if(System.String.IsNullOrEmpty(server))
            {
                server = "ws@" + System.Net.Dns.GetHostName();
            }

            string cookie = null;
            cookie = System.Configuration.ConfigurationManager.AppSettings["cookie"];
            if(System.String.IsNullOrEmpty(cookie))
            {
                cookie = "3ren";
            }

            OtpSelf cNode = new OtpSelf(Guid.NewGuid().ToString() + "@" + System.Net.Dns.GetHostName(), cookie);
            OtpPeer sNode = new OtpPeer(server);
            connection = cNode.connect(sNode);
        }
    }

    public static OtpConnection Connection
    {
        get
        {
            if (config == null)
            {
                config = new Config();
            }

            return config.connection;
        }
    }
}
