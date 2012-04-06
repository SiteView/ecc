

#include "stdafx.h"
#include "Wmi.h"
#include <erl_driver.h>
#include <ei.h>

extern  "C" { 
	int wmi_win(ei_x_buff *x,char *server,char *username,char *password,char *psql[],int count);
}



void initialize();
static vector<struct WmiNode> wminode;


void wmi_run(ei_x_buff *x,char *server,char *username,char *password,char *psql[],int count)
{
     CWmi  *m_wmi = new CWmi() ;
    if (m_wmi->Open(x,server,username,password))
	{
		struct WmiNode m_wminode;
		m_wminode.server = server;
		m_wminode.password = password;
		m_wminode.username = username;
		m_wminode.wmi = m_wmi;
		wminode.push_back(m_wminode);

		ei_x_encode_atom(x, "ok");
		for(int i = 0; i< count;i++)
		{
		    ei_x_encode_list_header(x,1);
			ei_x_encode_tuple_header(x, 2);	
		    ei_x_encode_string(x,psql[i]);
			m_wmi->Execute(x,psql[i]);

		}	
		ei_x_encode_empty_list(x);
		//m_wmi.Close();
	}
}

int wmi_win(ei_x_buff *x,char *server,char *username,char *password,char *psql[],int count)
{
	try
	{
		::std::string  strTemp = "";


		vector<struct WmiNode>::iterator iter = wminode.begin();
		for(iter; iter != wminode.end(); ++iter)
		{		
			if ((iter->server.compare(server) == 0) && (iter->username.compare(username) == 0) && (iter->password.compare(password) == 0))
			{		
				/*cout <<  "server:" << iter->server.c_str() << endl;
				cout <<  "username:" << iter->username.c_str() << endl;
				cout <<  "password:" << iter->password.c_str() << endl;*/
				
				ei_x_encode_atom(x, "ok");
				int ok = 0;
				for(int i = 0; i< count;i++)
				{
					ei_x_encode_list_header(x,1);
					ei_x_encode_tuple_header(x, 2);	
					ei_x_encode_string(x,psql[i]);
					if (iter->wmi->Execute(x,psql[i]))
					{
						ok++;
					}
				}	
				ei_x_encode_empty_list(x);				
				if (ok ==0) 
				{
					cout <<  "Server " << iter->server.c_str() << " Reconnected" << endl;
					iter->wmi->Close();
					iter = wminode.erase(iter);
					ei_x_free(x);				
					ei_x_new_with_version(x);
					ei_x_encode_tuple_header(x, 2);
					initialize();
					wmi_run(x,server,username,password,psql,count);
				}
					
				return 1;
			}
		}

		initialize();   

		wmi_run(x,server,username,password,psql,count);
	}
	catch(...) 
	{ 		
		ei_x_encode_atom(x, "error");		
		cout <<  "Server " << server << " Unknown error!" << endl;
		ei_x_encode_string(x, "Unknown error!");
		return false;
	} 

	return 1;
}


