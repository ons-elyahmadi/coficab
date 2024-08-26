import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SendTableCsvComponent } from './send-table-csv.component';

describe('SendTableCsvComponent', () => {
  let component: SendTableCsvComponent;
  let fixture: ComponentFixture<SendTableCsvComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SendTableCsvComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SendTableCsvComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
