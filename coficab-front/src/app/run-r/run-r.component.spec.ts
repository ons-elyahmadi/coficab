import { ComponentFixture, TestBed } from '@angular/core/testing';

import { RunRComponent } from './run-r.component';

describe('RunRComponent', () => {
  let component: RunRComponent;
  let fixture: ComponentFixture<RunRComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ RunRComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(RunRComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
