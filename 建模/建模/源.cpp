#include <iostream>
#include<cstdlib>
#include<vector>
#include<string>
#include<map>
#include <stack>
#include<utility>
#include<iterator>
#include<ctime>
#include <fstream>  
#include <sstream>  
#define math_e 2.71828
#define pi 3.1415926
#define ln3 log(3)/log(math_e)
#define random(x) rand()%(x)
#define xlnx(x) x>0?x*log(x)/log(math_e):0;

#define random_times 10
#define weight_PLK 0.22991
#define weight_FE 0.51856
#define weight_PF 0.241519
using namespace std;

double avg_PLK[62] = { 0 }, avg_FE[62] = {0},avg_PF[62] = { 0 };
int seats = 320;
int open_day;
vector<vector<int>> quo,req,today,done,all,quo_backup,empty_vec,avg_done,avg_all;
vector<vector<double>>lambda, poisson_sum,mu,sigma,norm_sum;
vector<int> temp; vector<double>temp_dbl;
vector<pair<int, int>> path;
map<pair<int, int>, double> dist;
long int factorial(int a) {
	if (a > 12)return INT_MAX;
	long int temp = 1;
	for (int i = 1; i <= a; i++) {
		 temp *= i;
	}
	return temp;
}
//void poisson_summing(int size,int days) {//算出所有days天每个路段的泊松分布函数离散值的和作为分母
//	for(int i=0;i<size;i++){
//		for (int k = 0; k < size; k++) {
//			if (lambda[i][k] != 0) {
//				for (int d = 0; d <= days; d++) {
//					poisson_sum[i][k] += (pow(lambda[i][k], d)*pow(math_e, -lambda[i][k])) / (factorial(d));
//					cout << poisson_sum[i][k];
//				}
//			}
//		}
//	}
//}
void norm_summing(int size, int days) {//算出所有days天每个路段的泊松分布函数离散值的和作为分母
	for (int i = 0; i<size; i++) {
		for (int k = 0; k < size; k++) {
			if (sigma[i][k] != 0 ) {
				for (int d = 0; d <= days; d++) {
					norm_sum[i][k] += 1/(sigma[i][k]*sqrt(2*pi))*pow(math_e,-(d-mu[i][k])*(d-mu[i][k])/2/sigma[i][k]/sigma[i][k]);
					//cout << norm_sum[i][k] << endl;
				}
			}
		}
	}
}
//void daily_poisson(int size,int day) {//算出第day日每个路段的实际需求值并存入today[][]中
//	for (int i = 0; i<size; i++) {
//		for (int k = 0; k < size; k++) {
//			if (poisson_sum[i][k] != 0) {
//					today[i][k] =floor((pow(lambda[i][k], day)*pow(math_e, -lambda[i][k])) / factorial(day) / poisson_sum[i][k] * req[i][k]);
//			}
//		}
//	}
//} 
void daily_norm(int size, int day) {//算出第day日每个路段的实际需求值并存入today[][]中
	for (int i = 0; i<size; i++) {
		for (int k = 0; k < size; k++) {
			if (norm_sum[i][k] != 0) {
				today[i][k] =round(1 / (sigma[i][k] * sqrt(2 * pi))*pow(math_e, -(day - mu[i][k])*(day - mu[i][k]) / 2.0 / sigma[i][k] / sigma[i][k])/norm_sum[i][k]*req[i][k]);
				all[i][k] += today[i][k];//实际每日总需求统计
			}
		}
	} 
}
double daily_norm2(int day,double sigma2,double mu2) {//算出正态分布实际值
	return (1 / (sigma2 * sqrt(2 * pi))*pow(math_e, -(day - mu2)*(day - mu2) / 2 / sigma2 / sigma2));
}

string bi_search(int start, int end,int date,int od) {
	string this_ticket = "("+to_string(start)+","+to_string(end)+"):";
	if (end - start == 1 and quo[start][end] == 0) { /*if (path.size() != 0)  path.pop_back();*/ return this_ticket+"failed_by_adjacent_ticket."; }
	if (quo[start][end] > 0) { /*path.push_back(pair<int, int>(start, end)); */quo[start][end]--; done[start][end]++; return this_ticket + "success_by_direct_ticket:"; }
	else {
		if (date<=od) {
			for (int i = start + 1; i < end; i++) {
				if (quo[start][i] > 0 and quo[i][end] > 0) {
					//path.push_back(pair<int, int>(start, i));
					//path.push_back(pair<int, int>(i, end));
					quo[start][i]--; quo[i][end]--;
					done[start][end]++;
					return(this_ticket + "success_by_joint_tickets:");

				}


			}
		}
		return(this_ticket + "failed_by_joint_ticket.");
	}
}
string handle_req(int start, int end,int num,int date,int od) {
	int flag=0;
	if (end - start < 1 or start<0 or end<0 or end>num - 1 or start>num - 2) { return "error_by_syntax" ; }
	return bi_search(start, end,date,od);
	for (int i = 0; i < num; i++) {
		for (int k = 0; k < num; k++) {
			if (quo[i][k] != 0)flag = 1;
		}
	}
	if (flag == 0) { return"all tickects sold out"; }
}
/*
string search(int start, int end) {
	if (end - start == 1 and quo[start][end] == 0) { if (path.size() != 0)  path.pop_back(); return "failed_by_adjacent_ticket"; }
	if (quo[start][end] > 0) { path.push_back(pair<int, int>(start, end)); return "success_by_direct_ticket"; }
	else {
		for (int i = start + 1; i < end; i++) {
			if (quo[start][i] > 0) {
				cout << "!" << endl;
				if (search(i, end) == "success_by_direct_ticket" or search(i, end) == "success_by_joint_ticket") {
					path.push_back(pair<int, int>(i, end));
					goto here;
				}
			}
		}
		path.pop_back();
		return("failed_by_joint_ticket");
	}
here:
	return("success_by_joint_ticket");
}
*/

double fair_entropy(vector<vector<int>> Y, vector<vector<int>>D,int size) {
	double factor1=0,factor2=0,sum_Y=0,sum_D=0;
	for (int i = 0; i < size; i++) {
		for (int k = 0; k < size;k++) {
			if (Y[i][k]) {
				sum_Y += Y[i][k]; sum_D += D[i][k];
			}
		}
	}
	for (int i = 0; i < size; i++) {
		for (int k = 0; k < size; k++) {
			if (Y[i][k]) {
				factor1 += xlnx(Y[i][k] / sum_Y);
				factor2 += xlnx((D[i][k] - Y[i][k]) / (sum_D - sum_Y));		
			}
		}
	}
	
	return -((sum_Y / sum_D)*(factor1) + (sum_D - sum_Y) / sum_D * (factor2));
}

double PLK(int size) {
	double RPK=0;
	for (int i = 0; i < size; i++) {
		for (int k = 0; k < size; k++) {
			if (quo_backup[i][k]) {
				RPK += (quo_backup[i][k] - quo[i][k])*dist[pair<int, int>(i,k)];
			}
		}
	}
	double ASK = seats * dist[pair<int, int>(0, size - 1)];
	return RPK / ASK;
}

int main() {
	ofstream outFile;
	outFile.open("data.csv", ios::out); // 打开模式可省略 
	srand((unsigned)time(NULL));
	string temp_str;

	int num;
	/*cout << "station num:";
	cin >> num;*/
	num = 3;
	//cout << "enter quota like [start,end,quota] end with -1"<<endl;
	for (int k = 0; k < num; k++) {
		temp.push_back(0);
		temp_dbl.push_back(0.0);
	}
	for (int i = 0; i < num; i++) {
		empty_vec.push_back(temp);
		all.push_back(temp);
		quo.push_back(temp);
		req.push_back(temp);
		lambda.push_back(temp_dbl);
		today.push_back(temp);
		poisson_sum.push_back(temp_dbl);
		norm_sum.push_back(temp_dbl);
		mu.push_back(temp_dbl);
		sigma.push_back(temp_dbl);
		done.push_back(temp);
	}
	avg_all = all;
	avg_done = done;
	int start, end, tic_num;

	quo[0][2] = 228;
	quo[0][1] = 71;
	quo[1][2] = 92;
	/*while (true) {
		cin >> start;
		if (start == -1)break;
		if (start<0 or start>num-1) { cout << "error syntax" << endl; continue; }
		cin >> end >> tic_num;
		if (end<0 or end>num or tic_num<0) { cout << "error syntax" << endl; continue; }
		quo[start][end] = tic_num;
	}*/

	//打印配额矩阵
	/*for (int i = 0; i < num; i++) {
		for (int k = 0; k < num; k++) {
			cout << quo[i][k] << " ";
		}
		cout << endl;
	}*/
	quo_backup = quo;
	/*cout << "enter requirements like [start,end,reqs] end with -1" << endl;
	while (true) {
		cin >> start;
		if (start == -1)break;
		if (start<0 or start>num - 2) { cout << "error syntax" << endl; continue; }
		cin >> end >> tic_num;
		if (end<0 or end>num-1 or tic_num<0) { cout << "error syntax" << endl; continue; }
		req[start][end] = tic_num;
	}*/

	//节假日：
	/*req[0][2] = 749;
	req[0][1] = 172;
	req[1][2] = 197;*/
	//平日:
	req[0][2] = 253;
	req[0][1] = 57;
	req[1][2] = 92

	/*req[0][2] = 300;
	req[0][1] = 10;
	req[1][2] = 10;*/
;
	/*cout << "enter distance like [start,end,distance] end with -1" << endl;
	while (true) {
		double distance;
		cin >> start;
		if (start == -1)break;
		if (start<0 or start>num - 2) { cout << "error syntax" << endl; continue; }
		cin >> end >> distance;
		if (end<0 or end>num - 1 or distance<0) { cout << "error syntax" << endl; continue; }
		dist[pair<int, int>(start, end)] = distance;
	}
	for (int i = 0; i < num; i++) {
		for (int k = i; k < num; k++) {
			if (k != i and dist[pair<int,int>(i,k)]==0) {
				for (int m = i; m < k;m++) {
					dist[pair<int, int>(i, k)] += dist[pair<int, int>(m,m+1)];
				}
			}
		}
	}*/
	dist[pair<int, int>(0, 1)] = 707;
	dist[pair<int, int>(0, 2)] = 1069;
	dist[pair<int, int>(1, 2)] = 362;


	/*cout << "enter items like [start,end,mu,sigma] end with -1" << endl;

	double m, s;
	while (true) {
		cin >> start;
		if (start == -1)break;
		if (start<0 or start>num - 1) { cout << "error syntax" << endl; continue; }
		cin >> end >> m >> s;
		if (end<0 or end>num) { cout << "error syntax" << endl; continue; }
		mu[start][end] = m;
		sigma[start][end] = s;
	}*/

	//节假日:
	/*mu[0][2] = 23.4; sigma[0][2] =6;
	mu[0][1] = 8.2; sigma[0][1] = 8.7;
	mu[1][2] = 12.1; sigma[1][2] = 7.5;*/
	//平日：
	mu[0][2] = 16; sigma[0][2] =5.645;
	mu[0][1] = 7.64; sigma[0][1] = 8.23;
	mu[1][2] = 11; sigma[1][2] = 7.23;

	cout << "enter days" << endl;
	int day; cin >> day;
	cout << "enter open_day" << endl;
	cin >> open_day;
	/*cout << "enter lambdas like [start,end,lambda] end with -1" << endl;
	double lam;
	while (true) {
	cin >> start;
	if (start == -1)break;
	if (start<0 or start>num - 1) { cout << "error syntax" << endl; continue; }
	cin >> end >> lam;
	if (end<0 or end>num or lam<0) { cout << "error syntax" << endl; continue; }
	lambda[start][end] = lam;
	}*/


	//poisson_summing(num, day);
	norm_summing(num, day);
	map<pair<int, int>, int> req_remain;
	map<pair<int, int>, int>::iterator it;
	int today_sum;
	for (int r_time = 0; r_time <random_times; r_time++) {//随机模拟次数
		for (int od = -1; od <= open_day; od++) {
			quo = quo_backup;
			//double sigma2, mu2; cin >> sigma2>>mu2;
			for (int d = day; d >= 0; d--) {
				//cout << "d" << d << ":" << daily_norm2(d,sigma2,mu2) << endl;   //测试正态分布值

				req_remain.clear();
				today_sum = 0;
				daily_norm(num, d);
				//cout << "days:" << d << endl;
				for (int i = 0; i < num; i++) {
					for (int k = 0; k < num; k++) {
						if (today[i][k] != 0) {
							today_sum += today[i][k];
							//cout << "(" << i << "," << k << "):" << today[i][k] << endl;
							req_remain[pair<int, int>(i, k)] = today[i][k];
						}
					}
				}
				/*it = req_remain.begin();
				while (it != req_remain.end()) {
				cout <<"("<<(it->first).first<<" "<<(it->first).second<<"):"<< it->second << endl;
				it++;
				}*/
				for (int i = today_sum; i > 0; i--) {
					int ran = random(i);
					/*cout << "ran:" << ran <<endl;*/
					it = req_remain.begin();
					while (it != req_remain.end()) {
						//cout << it->second << endl;
						if (it->second > ran) {
							//cout << handle_req((it->first).first, (it->first).second, num,d) << endl;
							temp_str = handle_req((it->first).first, (it->first).second, num, d, od);
							req_remain[it->first]--;
							break;
						}
						else {
							ran -= it->second; it++;
						}
					}


				}




			}
			//余票矩阵
			/*for (int i = 0; i < num; i++) {
				for (int k = 0; k < num; k++) {
					cout << quo[i][k] << " ";
				}
				cout << endl;
			}*/



			/*
			//old version
			int flag;
			while (true) {
				flag = 0;
				cin >> start >> end;
				if (end - start < 1 or start<0 or end<0 or end>num - 1 or start>num - 2) { cout << "error_by_syntax" << endl; continue; }
				cout << bi_search(start, end);
				while (path.size() != 0) { cout << "("<<path[path.size() - 1].first << "-" << path[path.size() - 1].second <<")"; path.pop_back(); }
				cout << endl;
				for (int i = 0; i < num; i++) {
					for (int k = 0; k < num; k++) {
						if (quo[i][k] != 0)flag = 1;
						cout << quo[i][k] << " ";
					}
					cout << endl;
				}
				cout << endl;
				if (flag==0) { cout << "all tickects sold out" << endl; break;  }
			}
			*/



			//cout << "open day:" << od << endl;
			//cout << "fair entropy:" << fair_entropy(done, all, num) << endl;
			//cout << "PLK:" << PLK(num) << endl;
			//cout << "travellers:" << travellers << endl;



			/*cout << "travel finished:" << endl;
			for (int i = 0; i < num; i++) {
				for (int k = 0; k < num; k++) {
					if (done[i][k]) { 
						cout << "[" << i << " " << k << "]:" << done[i][k] << endl; 
						
					}
				}
			}

			cout << "actual need:" << endl;
			for (int i = 0; i < num; i++) {
				for (int k = 0; k < num; k++) {
					if (all[i][k]) {
						cout << "[" << i << " " << k << "]:" << all[i][k] << endl;
						
					}
				}
			}*/

			double travellers = 0;
			for (int i = 0; i < num; i++) {
				for (int k = 0; k < num; k++) {
					if (done[i][k]) travellers += done[i][k];
				}
			}

			avg_FE[od+1] += fair_entropy(done,all,num)/ random_times;
			avg_PF[od+1] += travellers/random_times;
			avg_PLK[od+1] += PLK(num) /random_times;			


			done = empty_vec;
			all = empty_vec;
			//cout << endl;

		}
		//cout << "round " << r_time << " finished." << endl<<endl;
	}

	double sum_FE=0, sum_PF=0, sum_PLK=0;
	for (int i = 0; i <= open_day + 1; i++) {
		sum_FE += avg_FE[i];
		sum_PF += avg_PF[i];
		sum_PLK += avg_PLK[i];
	}

	double max_value = 0; int pos = 0;
	for (int i = 0; i <= open_day+1; i++) {
		outFile << i-1 << ',' << avg_FE[i]<< ',' << avg_PLK[i] << ',' << avg_PF[i] << ',' <<endl;
		if (avg_FE[i] /sum_FE* weight_FE + avg_PLK[i]/sum_PLK * weight_PLK +avg_PF[i]/sum_PF*weight_PF> max_value) {
			max_value = avg_FE[i] / sum_FE * weight_FE + avg_PLK[i] / sum_PLK * weight_PLK + avg_PF[i] / sum_PF * weight_PF;
			pos = i - 1;
		}
	}
	cout << "open-day:" << pos << endl;
	cout << "FE:" << avg_FE[pos] << "(weight=" << weight_FE << ");" << endl;
	cout << "PLK:" << avg_PLK[pos] << "(weight=" << weight_PLK << ");" << endl;
	cout << "PF:" << avg_PF[pos] << "(weight=" << weight_PF << ");" << endl;
	outFile.close();
	system("pause");
}