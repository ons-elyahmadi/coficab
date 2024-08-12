import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { HttpClientModule } from '@angular/common/http';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatTableModule } from '@angular/material/table';
import { MatRadioModule } from '@angular/material/radio';
 
import { AppComponent } from './app.component';
import { LoginComponent } from './login/login.component';
import { SignupComponent } from './signup/signup.component';
import { AppRoutingModule } from './app-routing.module';
import { DashboardComponent } from './dashboard/dashboard.component';
import { AgentDashboardComponent } from './agent-dashboard/agent-dashboard.component';
import { UserManagementComponent } from './user-management/user-management.component';
import { SendMailComponent } from './send-mail/send-mail.component';
 
import { PowerbiComponent } from './powerbi/powerbi.component';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSelectModule } from '@angular/material/select';
import { MatCardModule } from '@angular/material/card';
import { MatListModule } from '@angular/material/list';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatSnackBarModule } from '@angular/material/snack-bar';
import { ChatComponent } from './chat/chat.component';
import { SendTableCsvComponent } from './send-table-csv/send-table-csv.component';
import { RunRComponent } from './run-r/run-r.component';
import { TableComponent } from './table/table.component';
 
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDatepickerModule } from '@angular/material/datepicker';
 ;
import { MatNativeDateModule } from '@angular/material/core';
 
@NgModule({
  declarations: [
    AppComponent,
    LoginComponent,
    SignupComponent , 
    DashboardComponent, AgentDashboardComponent, UserManagementComponent, SendMailComponent , PowerbiComponent, ChatComponent, SendTableCsvComponent, RunRComponent, TableComponent
  ],
  imports: [
    BrowserModule,
    MatRadioModule ,
    FormsModule,
    MatTableModule,
    MatCheckboxModule,
    MatIconModule,
    BrowserAnimationsModule,
    ReactiveFormsModule,
    HttpClientModule,MatDatepickerModule,
    MatInputModule,
    MatFormFieldModule,
    MatNativeDateModule,
    BrowserAnimationsModule,
    MatInputModule,
    MatButtonModule,
    MatCardModule,
    MatFormFieldModule,
    MatListModule,
    MatButtonModule,
    MatInputModule,
    MatIconModule,
    MatSelectModule,
    MatCardModule,
    MatListModule,
    MatTableModule,
    MatFormFieldModule,
    MatToolbarModule,
    MatSnackBarModule,
    AppRoutingModule,
    BrowserModule,
    BrowserAnimationsModule,
    MatFormFieldModule,
    MatSelectModule,
    MatIconModule,
    MatTableModule
    
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
