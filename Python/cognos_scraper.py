__author__ = 'rhouckes'

import os, sys
from datetime import datetime
from bs4 import BeautifulSoup
#from bs4.element import CData

#from bs4 import BeautifulSoup
#import requests
import urllib
import urllib2
import cookielib
import mechanize
import urlparse
import re
import collections
import copy
import json

script_name = os.path.basename(__file__)
script_path = os.path.dirname(os.path.abspath(__file__))
autoload_path = os.path.abspath(os.path.join(script_path, '../../'))
util_path = os.path.abspath(os.path.join(autoload_path, 'utilities'))
sys.path.append(util_path)
from AutoLoad import AutoLoad
AutoLoad(autoload_path, ['lib', 'utilities'])
opt
from NetAuth import NetAuth
from CmdLine import CmdLine

class CognosScraper():

    arr_return_type = ['by_column','by_row', 'raw_json']
    return_type = 'by_row'
    base_url = 'http://cognos.imm.corp'
    post_url = 'http://cognos.imm.corp/ibmcognos/cgi-bin/cognos.cgi'
    report_list_url = 'http://cognos.imm.corp/ibmcognos/cgi-bin/cognos.cgi?encoding=UTF-8&b_action=xts.run&m=portal%2fcc.xts&m_folder=iA884E21E821D4CF8B66915827B1C195F'
    #report_name = 'Marder Directional GM By Pub'
    dict_url_params = {'run.prompt':'false'}
    #'ui.action':'forward'
    report_url = None
    arr_col_map = []
    dict_data_by_column = collections.OrderedDict()
    arr_data_by_row = []
    arr_column_index = []
    account_name = None
    campaign_name = None

    def __init__(self, report_name):
        self.report_name = report_name

        cj = cookielib.CookieJar()
        self.br = mechanize.Browser()
        self.br.set_cookiejar(cj)

        auth = NetAuth.get_auth_by_server('cognos.imm.corp')
        self.username = auth['login']
        self.password = auth['password']

    def set_return_type(self, return_type):
        if return_type is None:
            return
        if return_type not in self.arr_return_type:
            raise Exception('Invalid return type ['+return_type+'] valid types ['+str(self.arr_return_type)+']')
        self.return_type = return_type

    def set_date_range(self, str_start_date, str_end_date):
        print 'str_start_date: '+str_start_date+' str_end_date: '+str_end_date
        p_Date = '''<selectChoices><selectBoundRange selected="true"><start useValue="'''+str_start_date+'''" displayValue=""/><end useValue="'''+str_end_date+'''" displayValue=""/></selectBoundRange></selectChoices>'''
        self.dict_url_params['p_Date'] = p_Date

    def set_account(self, account_name):
        if campaign_name is None:
            return
        self.account_name = account_name
        p_Account = '''<selectChoices><selectOption useValue="'''+account_name+'''" displayValue="'''+account_name+'''"/></selectChoices>'''
        self.dict_url_params['p_Account'] = p_Account

    def set_campaign(self, campaign_name):
        if campaign_name is None:
            return
        self.campaign_name = campaign_name
        p_Campaign = '''<selectChoices><selectOption useValue="'''+campaign_name+'''" displayValue="'''+campaign_name+'''"/></selectChoices>'''
        self.dict_url_params['p_Campaign'] = p_Campaign

    def parse_params(self, params):
        if params is None:
            return
        arr_params = params.split(',')
        print 'params: '+str(arr_params)
        for param in arr_params:
            pair = param.split(':')
            print 'pair: '+str(pair)
            key = pair[0]
            try:
                val = pair[1]
            except:
                val = None
            self.__add_url_param(key, val)

    def __add_url_param(self, param_name, param_value):
        self.dict_url_params[param_name] = param_value

    def __get_url_params(self):
        url_parts = list(urlparse.urlparse(self.report_url))
        query = dict(urlparse.parse_qsl(url_parts[4]))
        query.update(self.dict_url_params)
        url_parts[4] = urllib.urlencode(query)
        self.report_url = urlparse.urlunparse(url_parts)
        print 'updated report_url query: '+str(self.report_url)

    def __post_data(self, post_url, dict_params):
        self.br.select_form(nr=0)
        for key, val in dict_params.iteritems():
            self.br.form['key'] = val
        response = self.br.submit()
        #post_data = urllib.urlencode(dict_params)
        #response = self.br.open(post_url, post_data)
        return response

    def __do_login(self):
        self.br.select_form(nr=0)
        self.br.form['CAMUsername'] = self.username
        self.br.form['CAMPassword'] = self.password
        self.br.submit()
        #print br.response().read()

    # def do_login(self):
    #     payload = {'CAMUsername' : username, 'CAMPassword' : password}
    #
    #     # Use urllib to encode the payload
    #     data = urllib.urlencode(payload)
    #
    #     # Build our Request object (supplying 'data' makes it a POST)
    #     req = urllib2.Request(self.post_url, data)
    #
    #     # Make the request and read the response
    #     resp = urllib2.urlopen(req)
    #     #contents = resp.read()
    #     #print 'login reposne: '+str(contents)
    #     return get_page_soup(report_url)

    def __get_page_soup(self, str_url):
        page = urllib2.urlopen(str_url)
        str_html = page.read()
        print 'page html: '+str(str_html)
        return BeautifulSoup(str_html, "html5lib")

    # def do_login():
    #     response = requests.get(report_list_url)
    #     print 'request url {rqst} response url {resp}'.format(rqst=report_list_url, resp=response.url)
    #     r = requests.get('https://api.github.com/user', auth=('user', 'pass'))
    #     r.status_code
    #     r.text

    def __get_soup(self, str_html):
        return BeautifulSoup(str_html, "html5lib")

    def __is_logged_in(self, str_html):
        result = True
        soup = self.__get_soup(str_html)
        user_field = soup.find(id='CAMUsername')
        if user_field is not None:
            result = False
        return result

    # find the report url
    def __get_report_url(self):
        self.report_url = None

        # go to the report list page
        response = self.br.open(self.report_list_url)
        str_html = response.read()

        if not self.__is_logged_in(str_html):
            self.__do_login()
            response = self.br.open(self.report_list_url)
            str_html = response.read()

        soup = self.__get_soup(str_html)

        # find all the report entries on the page
        arr_table_elem = soup.find_all('td', {'class': 'tableText'})
        #print 'content: '+str(arr_table_elem)
        for table_elem in arr_table_elem:
            anchor = table_elem.find('a')
            if anchor is not None and anchor.string == self.report_name:
                self.report_url = anchor.get('href', None)

        # make sure the url has the base url
        if self.base_url not in self.report_url:
            self.report_url = urlparse.urljoin(self.base_url, self.report_url)

        # update the url query string params
        self.__get_url_params()

        #print 'report_url: '+str(self.report_url)
        return self.report_url

    # get the header/column names
    def __get_column_map(self):
        index = 0
        for data in self.arr_data_elem:
            try:
                type = str(data.parent['type'])
            except:
                #print 'type is invalid ['+str(data)+'] at index ['+str(index)+'] parent ['+str(data.parent)+']'
                continue

            text = str(data.string)

            if type == 'columnTitle':
                self.arr_column_index.append(index)
                if text not in self.arr_col_map:
                    self.arr_col_map.append(text)
                #print 'processing column data ['+str(data)+'] at index ['+str(index)+']'
            index += 1

        self.arr_column_index.reverse()
        # remove the columns from the data
        for column_index in self.arr_column_index:
            #print 'deleting column data ['+str(data)+'] at index ['+str(column_index)+']'
            del self.arr_data_elem[column_index]

        #print 'arr_data_elem: '+str(self.arr_data_elem)

    # get the data organized by column
    def __get_data_by_column(self):
        # get the column count
        col_count = len(self.arr_col_map)

        if col_count == 0:
            print 'No data found, header count ['+str(col_count)+'] '
            return None

        # initialize the data by column dictionary
        for col_name in self.arr_col_map:
            if not self.dict_data_by_column.has_key(col_name):
                    self.dict_data_by_column[col_name] = []

        index = 0
        for data in self.arr_data_elem:
            try:
                type = str(data.parent['type'])
            except:
                continue

            text = str(data.string)

            if type == 'datavalue':
                cid = int(data.parent['cid'])
                try:
                    col_name = self.arr_col_map[cid]
                except:
                    print 'EXCEPTION: arr_col_map: '+str(self.arr_col_map)+' cid: '+str(cid)+' dict_data_by_column: '+str(self.dict_data_by_column)
                    raise
                self.dict_data_by_column[col_name].append(text)
            else:
                raise Exception('unknown type: '+type+' at index: '+str(index))
            index += 1

        #print 'dict_data_by_column: '+str(self.dict_data_by_column)
        return json.dumps(self.dict_data_by_column)

    # get the data organized by row
    def __get_data_by_row(self):
        # get the column count
        col_count = len(self.arr_col_map)

        if col_count == 0:
            print 'No data found, header count ['+str(col_count)+'] '
            return None

        # make a copy of the data
        arr_data_elem = copy.copy(self.arr_data_elem)
        #print 'arr_data_elem: '+str(arr_data_elem)

        # add the header to the data by row
        self.arr_data_by_row.append(self.arr_col_map)

        while len(arr_data_elem) > 0:
            # slice the data elements by column count
            arr_data_elem_row = arr_data_elem[:col_count]

            # remove the sliced data elements
            del arr_data_elem[:col_count]

            # get the text value from the data element
            arr_data_row = []
            for data_elem in arr_data_elem_row:
                arr_data_row.append(str(data_elem.string))

            # add the row
            if len(arr_data_row) == col_count:
                self.arr_data_by_row.append(arr_data_row)

        #print 'arr_data_by_row: '+str(self.arr_data_by_row)
        return json.dumps(self.arr_data_by_row)

    # get the raw json data
    def __get_json_data(self, soup):
        json_obj_start = 'window.oCVSC_NS_.addContextData('
        json_obj_end = ');'
        json = None
        for x in soup.find_all(text=re.compile("CDATA")):
            x = x.replace('//' ,'')
            x = x.replace('<![CDATA[', '').replace(']]>', '')
            text = str(x).strip()
            if json_obj_start in text:
                json = text.replace(json_obj_start, '').replace(json_obj_end, '')
        #print 'json: '+str(json)
        return json

    def __add_soup_element(self, soup_elem, str_new_elem):
        temp_soup = BeautifulSoup(str_new_elem, "html5lib")
        # BeautifulSoup automatically add <html> and <body> tags
        new_tag = temp_soup.html.body.contents[0]
        #new_tag = temp_soup.html.body.form.find({'name':'str_new_elem'})
        soup_elem.insert(0, new_tag)

    def execute(self):
        # get the report url
        self.__get_report_url()

        if self.report_url is None:
            raise Exception('Invalid report url')

        # open the report and get the data
        response = self.br.open(self.report_url)
        str_html = response.read()
        #print 'report response: '+str(str_html)

        soup = self.__get_soup(str_html)

        self.arr_data_elem = soup.find_all('span', {'class':'textItem'})

        self.__get_column_map()

        if self.return_type == 'by_column':
            json_data = self.__get_data_by_column()
        elif self.return_type == 'by_row':
            json_data = self.__get_data_by_row()
        elif self.return_type == 'raw_json':
            json_data = self.__get_json_data(soup)
        else:
            raise Exception('unknown return type: '+str(self.return_type))

        print 'json data: '
        print json_data

#######################################################################################################################

#if __name__ == '__main__':

cmdline = CmdLine()
cmdline.add_opt(short_name='r', long_name='report_name', opt_required=True, val_required=True, opt_desc="report name (i.e. 'Marder Directional GM By Pub'")
cmdline.add_opt(short_name='t', long_name='return_type', opt_required=False, val_required=True, opt_desc='return type (i.e. by_row | by_column | raw_json) default: by_row')
cmdline.add_opt(short_name='d', long_name='date_range', opt_required=False, val_required=True, opt_desc='date range (i.e. 2016-04-28:2016-04-30) default: <todays date>:<todays date>')
cmdline.add_opt(short_name='a', long_name='account_name', opt_required=False, val_required=True, opt_desc="account name (i.e. '5Z-Straight Talk AWS - 2016 Tax Time')")
cmdline.add_opt(short_name='c', long_name='campaign_name', opt_required=False, val_required=True, opt_desc="campaign name (i.e. 'Big 5 RTG 2015')")
cmdline.add_opt(short_name='p', long_name='params', opt_required=False, val_required=True, opt_desc='url get params (i.e. \'p_Date:p_date:<selectChoices><selectBoundRange selected="true"><start useValue="2016-04-28" displayValue=""/><end useValue="2016-04-30" displayValue=""/></selectBoundRange></selectChoices>\'')
cmdline.parse()

report_name = cmdline.get_parsed_value('-r')
date_range = cmdline.get_parsed_value('-d')
return_type = cmdline.get_parsed_value('-t')
account_name = cmdline.get_parsed_value('-a')
campaign_name = cmdline.get_parsed_value('-c')
params = cmdline.get_parsed_value('-p')

if date_range is not None:
    arr_date_range = date_range.split(':')

# set the start date to today or from user input
str_start_date = str(datetime.now().date())
try:
    str_start_date = arr_date_range[0]
except:
    pass

# set the end date to today or from user input
str_end_date = date = str(datetime.now().date())
try:
    str_end_date = arr_date_range[1]
except:
    pass

start_date = datetime.strptime(str_start_date, '%Y-%m-%d')
end_date = datetime.strptime(str_end_date, '%Y-%m-%d')

scraper = CognosScraper(report_name)
scraper.set_return_type(return_type)
scraper.set_date_range(str_start_date, str_end_date)
scraper.set_account(account_name)
scraper.set_campaign(campaign_name)
scraper.parse_params(params)
scraper.execute()




