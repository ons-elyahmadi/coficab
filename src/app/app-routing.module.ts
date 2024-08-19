import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AgentDashboardComponent } from './agent-dashboard/agent-dashboard.component';
import { DashboardComponent } from './dashboard/dashboard.component';
import { LoginComponent } from './login/login.component';
import { SendMailComponent } from './send-mail/send-mail.component';
import { SignupComponent } from './signup/signup.component';
import { UserManagementComponent } from './user-management/user-management.component';
 
import { AuthGuard } from './auth.guard';
import { ChatComponent } from './chat/chat.component';
import { PowerbiComponent } from './powerbi/powerbi.component';
import { RunRComponent } from './run-r/run-r.component';
import { SendTableCsvComponent } from './send-table-csv/send-table-csv.component';
import { TableComponent } from './table/table.component';

const routes: Routes = [
  { path: 'agent-dashboard', component: AgentDashboardComponent , canActivate: [AuthGuard],
    data: { role: 'Agent' } , children: [
    { path: 'user-management', component: UserManagementComponent },
    
    { path: 'chat', component: ChatComponent },
    { path: 'send-mailtable', component: SendTableCsvComponent },
    { path: 'run-r', component: RunRComponent },
    { path: 'powerbi', component: PowerbiComponent },
    { path: 'tables', component: TableComponent }
  ]
},
{ path: 'dashboard', component: DashboardComponent, canActivate: [AuthGuard] ,
  data: { role: 'User' }, children: [
  
  { path: 'send-mail', component: SendMailComponent },
  { path: 'chat', component: ChatComponent },
  { path: 'send-mailtable', component: SendTableCsvComponent },
  { path: 'run-r', component: RunRComponent },
  { path: 'powerbi', component: PowerbiComponent },
  { path: 'tables', component: TableComponent }
]
},
  { path: '', redirectTo: '/login', pathMatch: 'full' },
  { path: 'login', component: LoginComponent },
  { path: 'signup', component: SignupComponent },
  { path: 'dashboard', component: DashboardComponent },
  
  { path: 'agent-dashboard', component: AgentDashboardComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
